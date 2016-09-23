open Utils
open Globals

open HardCaml
open Signal.Types
open Cyclesim.Api 
module Sc = Signal.Comb
module St = Signal.Types
module Cs = Cyclesim

module Ee = Llvm_executionengine.ExecutionEngine
module Gv = Llvm_executionengine.GenericValue

let debug = false

type cyclesim = Bits_ext.Comb.BigarraybitsNativeint.t Cyclesim.Api.cyclesim

let reset_value = function
  | Signal_reg(_,r) ->
    let v = try const_of_signal r.reg_reset_value 
      with _ -> failwith "register reset values must be constant in the llvm code generator"
    in
    v
  | _ -> failwith "Expecting register"

let split max b = 
  let rec split n a b = 
    if n=max then List.rev a, b
    else
      match b with
      | [] -> List.rev a,[]
      | h::t -> split (n+1) (h::a) t
  in
  split 0 [] b

(* initialise the compilation module *)
let compile_init circuit = 
  let context = global_context () in
  let modl = Llvm.create_module context (Circuit.name circuit) in

  let gfn = Globals.global_fns modl in

  (* information gathering *)
  Info.compile_width modl "i" (Circuit.inputs circuit);
  Info.compile_width modl "o" (Circuit.outputs circuit);
  Info.compile_name modl "i" (Circuit.inputs circuit);
  Info.compile_name modl "o" (Circuit.outputs circuit);
  Info.compile_ptr (gfn.gsimple true) modl "i" (Circuit.inputs circuit);
  Info.compile_ptr (gfn.gsimple true) modl "o" (Circuit.outputs circuit);

  (* scheduler *)
  let regs, mems, consts, inputs, remaining = Cs.find_elements circuit in
  let ready = regs @ inputs @ consts in
  let deps' s = 
      match s with 
      | Signal_mem(_, _,  _, m) -> [m.mem_read_address]
      | _ -> deps s
  in
  let schedule = Cs.scheduler deps' (mems @ remaining) ready in

  (* initial creation of regs and mems (inputs and outputs already made) *)
  List.iter (fun s -> gfn.greg (Sc.width s) s |> ignore) regs;
  List.iter (fun s -> gfn.gmem (Sc.width s) (memsize s) s |> ignore) mems;
  
  modl, gfn, schedule, regs, mems

(* compile combinatorial signals *)
let compile_cycle modl gfn signals fn = 
  (if debug then List.iter (fun s -> Printf.printf "sig=%s\n" (to_string s)) signals);

  Llvm.set_linkage Llvm.Linkage.Internal fn.func;
  let gfn = gfn.fscope fn in
  let map = Compile.compile_comb_list 
      modl gfn fn.func fn.builder 
      UidMap.empty signals
  in
  let store s = 
    try (*gfn.stores.ssimple (load_signal gfn map s) false s*)
      store_signal gfn (snd (load_signal gfn map s)) s
    with _ -> ()
  in
  let return () = 
    Llvm.build_ret_void fn.builder |> ignore;
    assert_valid_function fn.func;
  in
  fn.func, map, store, return 

(* sort out inter-sub-cycle dependancies and return from the functions *)
let compile_cycle_deps circuit g_map r = 
  let is_input = 
    let set = List.fold_left (fun m s -> UidSet.add (uid s) m)
      UidSet.empty (Circuit.inputs circuit) in
    (fun u -> UidSet.mem u set)
  in
  List.iter (fun (_,m,store,_) ->
    UidMap.iter (fun u _ ->
      let s = Circuit.signal_of_uid circuit u in
      let find u = 
        try UidMap.find u !g_map |> ignore; true
        with _ -> false
      in
      if (find u) &&
          (not (is_input u)) && 
          (not (is_reg s)) && 
          (not (is_mem s)) then
        store s
    ) m
  ) r;
  List.iter (fun (_,_,_,r) -> r()) r

(* compile register store *)
let compile_reg_store gfn signals fn = 
  Llvm.set_linkage Llvm.Linkage.Internal fn.func;
  let gfn = gfn.fscope fn in
  let load map signal = snd (load_signal gfn map signal) in
  let store = gfn.stores.sreg in
  let compile_reg = Compile.compile_reg fn.builder (load UidMap.empty) store in
  List.iter compile_reg signals;
  Llvm.build_ret_void fn.builder |> ignore;
  fn.func

(* compile memory store *)
let compile_mem_store gfn signals fn = 
  Llvm.set_linkage Llvm.Linkage.Internal fn.func;
  let gfn = gfn.fscope fn in
  let load map signal = snd (load_signal ~rd_mem:false gfn map signal) in
  let store = gfn.stores.smem in
  let compile_mem = Compile.compile_mem fn.builder (load UidMap.empty) store in
  List.iter compile_mem signals;
  Llvm.build_ret_void fn.builder |> ignore;
  fn.func

(* compile register update *)
let compile_reg_update gfn signals fn = 
  Llvm.set_linkage Llvm.Linkage.Internal fn.func;
  let u_reg = (gfn.fscope fn).updates.ureg in
  List.iter u_reg signals;
  Llvm.build_ret_void fn.builder |> ignore;
  assert_valid_function fn.func;
  fn.func

(* compile register update *)
let compile_mem_update gfn signals fn = 
  Llvm.set_linkage Llvm.Linkage.Internal fn.func;
  let gfn = gfn.fscope fn in
  List.iter (function
    | Signal_mem(_,_,_,m) as signal -> 
      let _, addr = load_signal gfn UidMap.empty m.mem_write_address in
      gfn.updates.umem addr signal
    | _ -> failwith "expecting memory") signals;
  Llvm.build_ret_void fn.builder |> ignore;
  assert_valid_function fn.func;
  fn.func

(* compile register reset *)
let compile_reg_reset gfn regs fn = 
  List.iter (fun s ->
      let v = reset_value s in
      let g = gfn.greg (Sc.width s) s in
      ignore (Llvm.build_store v g.Globals.cur fn.builder) 
    ) regs;
  Llvm.build_ret_void fn.builder |> ignore;
  assert_valid_function fn.func 

let call_void_fns modl name fns = 
  make_function modl name void [||] (fun efn ->
    List.iter (fun f -> Llvm.build_call f [||] "" efn.builder |> ignore) fns; 
    Llvm.build_ret_void efn.builder |> ignore;
    assert_valid_function efn.func)

let compile max circuit = 

  let modl, gfn, schedule, regs, mems = compile_init circuit in

  let rec build name n f signals = 
    let h,t = split max signals in
    let r = make_function modl (name ^ "_" ^ string_of_int n) void [||] (f h) in
    if t=[] then [r]
    else r :: build name (n+1) f t
  in

  (* build the sub-cycle functions *)
  let comb = build "cycle" 0 (compile_cycle modl gfn) schedule in

  (* build register updates *)
  let reg_store = build "reg_store" 0 (compile_reg_store gfn) regs in
  let reg_update = build "reg_update" 0 (compile_reg_update gfn) regs in

  (* build memory updates *)
  let mem_store = build "mem_store" 0 (compile_mem_store gfn) mems in
  let mem_update = build "mem_update" 0 (compile_mem_update gfn) mems in

  (* sort out inter-sub-cycle dependancies *)
  compile_cycle_deps circuit gfn.gmap comb;

  let fcomb = List.map (fun (f,_,_,_) -> f) comb in
  call_void_fns modl "sim_cycle_comb0" (fcomb @ reg_store @ mem_store);
  call_void_fns modl "sim_cycle_seq" (mem_update @ reg_update);
  call_void_fns modl "sim_cycle_comb1" fcomb;

  make_function modl "sim_reset" void [||] (compile_reg_reset gfn regs) |> ignore;

  (* dump_module modl; *)
  modl

let compile' = compile 50

let make circuit = 
  let modl = compile' circuit in
  (*let mp = ModuleProvider.create modl in
    let jit = Ee.create_jit mp in*)
  let optlevel = 2 in
  let jit = Ee.create_jit modl optlevel in
  let mk name = 
    let f = Info.lookup_function name jit in
    (fun () -> Ee.run_function f [||] jit |> ignore) 
  in
  let sim_cycle_comb0 = mk "sim_cycle_comb0" in 
  let sim_cycle_seq = mk "sim_cycle_seq" in 
  let sim_cycle_comb1 = mk "sim_cycle_comb1" in 
  let sim_reset = mk "sim_reset" in
  let in_ports = Info.query_ports "i" jit in
  let out_ports = Info.query_ports "o" jit in
  {
    sim_internal_ports = [];
    sim_in_ports = in_ports;
    sim_out_ports = out_ports;
    sim_out_ports_next = out_ports;
    sim_cycle_check = (fun () -> ());
    sim_cycle_comb0;
    sim_cycle_seq;
    sim_cycle_comb1;
    sim_reset;
    sim_lookup_signal = (fun uid -> failwith "sim_lookup_signal not implemented");
    sim_lookup_reg = (fun uid -> failwith "sim_lookup_reg not implemented");
    sim_lookup_memory = (fun uid addr -> failwith "sim_lookup_memory not implemented");
  }

let write path circuit = 
  let modl = compile' circuit in
  (* write bitcode *)
  let fname = Filename.concat path (Circuit.name circuit ^ ".bc") in
  if not (Llvm_bitwriter.write_bitcode_file modl fname) then
    failwith "Failed to write bitcode"

let load path = 
  let name = Bits_ext.Utils_ext.filebase path in
  let context = global_context () in
  let membuf = Llvm.MemoryBuffer.of_file path in
  let modl = Llvm_bitreader.parse_bitcode context membuf in
  (* let mp = ModuleProvider.create modl in
     let jit = Ee.create_jit mp in *)
  let optlevel = 2 in
  let jit = Ee.create_jit modl optlevel in
  (* need to get the circuit information from the bit code *)
  let sim_cycle_comb0 = Info.lookup_function ("sim_cycle_comb0") jit in
  let sim_cycle_seq = Info.lookup_function ("sim_cycle_seq") jit in
  let sim_cycle_comb1 = Info.lookup_function ("sim_cycle_comb1") jit in
  let sim_reset = Info.lookup_function ("reset_" ^ name) jit in

  let in_ports = Info.query_ports "i" jit in
  let out_ports = Info.query_ports "o" jit in

  let sim_cycle_comb0 = (fun () -> ignore (Ee.run_function sim_cycle_comb0 [||] jit)) in
  let sim_cycle_seq = (fun () -> ignore (Ee.run_function sim_cycle_seq [||] jit)) in
  let sim_cycle_comb1 = (fun () -> ignore (Ee.run_function sim_cycle_comb1 [||] jit)) in
  let sim_reset = (fun () -> ignore (Ee.run_function sim_reset [||] jit)) in

  {
    sim_internal_ports = [];
    sim_in_ports = in_ports;
    sim_out_ports = out_ports;
    sim_out_ports_next = out_ports;
    sim_cycle_check = (fun () -> ());
    sim_cycle_comb0;
    sim_cycle_seq;
    sim_cycle_comb1;
    sim_reset;
    sim_lookup_signal = (fun uid -> failwith "sim_lookup_signal not implemented");
    sim_lookup_reg = (fun uid -> failwith "sim_lookup_reg not implemented");
    sim_lookup_memory = (fun uid addr -> failwith "sim_lookup_memory not implemented");
  }

module Make(B : Bits_ext.S) =
struct

  type t = B.t
  type base_cyclesim = cyclesim
  type cyclesim = t Cyclesim.Api.cyclesim

  let wrap sim = 
    let in_ports = List.map (fun (s,d) -> 
      let d,w = fst !d, snd !d in s, (ref (B.zero w), d, w)) sim.sim_in_ports 
    in
    
    let out_ports = List.map (fun (s,d) -> 
      let d,w = fst !d, snd !d in s, (ref (B.zero w), d, w)) sim.sim_out_ports 
    in
    let out_ports_next = List.map (fun (s,d) -> 
      let d,w = fst !d, snd !d in s, (ref (B.zero w), d, w)) sim.sim_out_ports 
    in
    
    let update_in_ports () = 
      List.iter 
        (fun (s,(t,b,w)) ->
          if B.width !t <> w then 
            failwith 
              (Printf.sprintf "input port '%s' width mismatch: expected %i got %i" 
                s w (B.width !t))
          else
            B.to_bani_ptr !t b)
        in_ports
    in
    let update_out_ports out_ports = 
      List.iter (fun (s,(t,b,w)) -> t := B.of_bani_ptr w b (B.zero w)) out_ports
    in

    let sim_cycle_check = sim.sim_cycle_check in
    let sim_cycle_comb0 () = update_in_ports (); sim.sim_cycle_comb0 (); update_out_ports out_ports in
    let sim_cycle_seq = sim.sim_cycle_seq in
    let sim_cycle_comb1 () = sim.sim_cycle_comb1(); update_out_ports out_ports_next in
    let sim_reset () = (*update_in_ports ();*) sim.sim_reset (); (*update_out_ports ()*) in

    {
      sim_internal_ports = [];
      sim_in_ports = List.map (fun (s,(t,b,w)) -> s,t) in_ports;
      sim_out_ports = List.map (fun (s,(t,b,w)) -> s,t) out_ports;
      sim_out_ports_next = List.map (fun (s,(t,b,w)) -> s,t) out_ports_next;
      sim_cycle_check;
      sim_cycle_comb0;
      sim_cycle_seq;
      sim_cycle_comb1;
      sim_reset;
      sim_lookup_signal = (fun uid -> failwith "sim_lookup_signal not implemented");
      sim_lookup_reg = (fun uid -> failwith "sim_lookup_reg not implemented");
      sim_lookup_memory = (fun uid addr -> failwith "sim_lookup_memory not implemented");
    }

  let make circuit = wrap (make circuit)

  let write path circuit = write path circuit

  let load path = wrap (load path)

end

module Gen(B : Bits_ext.S)(I : Interface.S)(O : Interface.S) = struct

    module S = Make(B)
    module Cs = Cyclesim.Api

    let make name logic = 
        let outputs = logic I.(map (fun (n,b) -> Signal.Comb.input n b) t) in 
        let circuit = Circuit.make name 
            (O.to_list (O.map2 (fun (n,_) s -> Signal.Comb.output n s) O.t outputs))
        in
        let sim = S.make circuit in
        let inputs = I.(map (fun (n,_) -> try Cs.in_port sim n with _ -> ref B.empty) t) in
        let outputs = O.(map (fun (n,_) -> try Cs.out_port sim n with _ -> ref B.empty) t) in
        circuit, sim, inputs, outputs

end

(* install as sim provider *)

let () = 
  HardCamlDynlink.Sim_provider.(add_provider "hardcaml-llvmsim" (module Make : S))

