open Utils

open HardCaml
open Signal.Types
open Cyclesim.Api 
module Sc = Signal.Comb
module St = Signal.Types
module Cs = Cyclesim

module Ee = Llvm_executionengine.ExecutionEngine
module Gv = Llvm_executionengine.GenericValue

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

  let g_map,g_ops = Globals.globals modl in
  let g_simple,g_reg,g_mem = g_ops in

  (* information gathering *)
  Info.compile_width modl "i" (Circuit.inputs circuit);
  Info.compile_width modl "o" (Circuit.outputs circuit);
  Info.compile_name modl "i" (Circuit.inputs circuit);
  Info.compile_name modl "o" (Circuit.outputs circuit);
  Info.compile_ptr (g_simple true) modl "i" (Circuit.inputs circuit);
  Info.compile_ptr (g_simple true) modl "o" (Circuit.outputs circuit);

  (* scheduler *)
  let regs, mems, consts, inputs, remaining = Cs.find_elements circuit in
  let ready = regs @ mems @ inputs @ consts in
  let schedule = Cs.scheduler deps remaining ready in

  (* initial creation of regs and mems (inputs and outputs already made) *)
  List.iter (fun s -> g_reg (Sc.width s) (uid s) |> ignore) regs;
  List.iter (fun s -> g_mem (Sc.width s) (memsize s) (uid s) |> ignore) mems;
  
  modl, g_map, g_ops, schedule, regs, mems

(* compile combinatorial signals *)
let compile_cycle modl g_ops signals fn = 
  List.iter (fun s -> Printf.printf "sig=%s\n" (to_string s)) signals;

  Llvm.set_linkage Llvm.Linkage.Internal fn.func;
  let l_ops = Globals.load fn g_ops in
  let load = Globals.load_signal l_ops in
  let s_simple,_,_ = Globals.store fn.builder g_ops in
  let map = Compile.compile_comb_list 
      modl fn.func fn.builder load
      UidMap.empty signals
  in
  let store rnd s = 
    try s_simple (load map s) rnd s 
    with _ -> ()
  in
  let return () = 
    Llvm.build_ret_void fn.builder |> ignore;
    Llvm_analysis.assert_valid_function fn.func |> ignore;
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
        store false s
    ) m
  ) r;
  List.iter (fun (_,_,_,r) -> r()) r

(* compile register store *)
let compile_reg_store g_ops signals fn = 
  Llvm.set_linkage Llvm.Linkage.Internal fn.func;
  let l_ops = Globals.load fn g_ops in
  let load = Globals.load_signal l_ops in
  let _,s_reg,_ = Globals.store fn.builder g_ops in
  let compile_reg = Compile.compile_reg fn.builder 
      (load UidMap.empty) s_reg in
  List.iter compile_reg signals;
  Llvm.build_ret_void fn.builder |> ignore;
  fn.func

(* compile register update *)
let compile_reg_update g_ops signals fn = 
  Llvm.set_linkage Llvm.Linkage.Internal fn.func;
  let u_reg,_ = Globals.update fn.builder g_ops in
  List.iter u_reg signals;
  Llvm.build_ret_void fn.builder |> ignore;
  Llvm_analysis.assert_valid_function fn.func |> ignore;
  fn.func

(* compile register reset *)
let compile_reset g_reg regs fn = 
  List.iter (fun s ->
      let v = reset_value s in
      let g = g_reg (Sc.width s) (uid s) in
      ignore (Llvm.build_store v g.Globals.cur fn.builder) 
    ) regs;
  Llvm.build_ret_void fn.builder |> ignore;
  Llvm_analysis.assert_valid_function fn.func |> ignore

let call_void_fns modl name fns = 
  make_function modl name void [||] (fun efn ->
    List.iter (fun f -> Llvm.build_call f [||] "" efn.builder |> ignore) fns; 
    Llvm.build_ret_void efn.builder |> ignore;
    Llvm_analysis.assert_valid_function efn.func |> ignore)

let compile_simple circuit = 

  let modl, g_map, g_ops, schedule, regs, mems = compile_init circuit in
  let g_simple,g_reg,g_mem = g_ops in

  let compile_cycle fn = 
    let fn,map,store,return = compile_cycle modl g_ops schedule fn in 
    compile_cycle_deps circuit g_map [(fn,map,store,return)];
    fn
  in

  let reg_store = make_function modl "reg_store" void [||] (compile_reg_store g_ops regs) in
  let reg_update = make_function modl "reg_update" void [||] (compile_reg_update g_ops regs) in
  let comb = make_function modl "cycle_comb" void [||] compile_cycle in

  call_void_fns modl "sim_cycle_comb0" [ comb; reg_store ];
  call_void_fns modl "sim_cycle_seq" [ reg_update ];
  call_void_fns modl "sim_cycle_comb1" [ comb ];

  make_function modl "sim_reset" void [||] (compile_reset g_reg regs);

  (*Llvm.dump_module modl;*)
  modl

let compile max circuit = 

  let modl, g_map, g_ops, schedule, regs, mems = compile_init circuit in
  let g_simple,g_reg,g_mem = g_ops in

  let rec build name n f signals = 
    let h,t = split max signals in
    let r = make_function modl (name ^ "_" ^ string_of_int n) void [||] (f h) in
    if t=[] then [r]
    else r :: build name (n+1) f t
  in

  (* build the sub-cycle functions *)
  Printf.printf "compiling combinatorial [%i]\n%!" (List.length schedule);
  let comb = build "cycle" 0 (compile_cycle modl g_ops) schedule in

  (* build register updates *)
  Printf.printf "compiling seqential (store) [%i]\n%!" (List.length regs);
  let reg_store = build "reg_store" 0 (compile_reg_store g_ops) regs in
  Printf.printf "compiling seqential (update) [%i]\n%!" (List.length regs);
  let reg_update = build "reg_update" 0 (compile_reg_update g_ops) regs in

  (* sort out inter-sub-cycle dependancies *)
  Printf.printf "compiling dependants [%i]\n%!" (List.length comb);
  compile_cycle_deps circuit g_map comb;

  let fcomb = List.map (fun (f,_,_,_) -> f) comb in
  call_void_fns modl "sim_cycle_comb0" (fcomb @ reg_store);
  call_void_fns modl "sim_cycle_seq" reg_update;
  call_void_fns modl "sim_cycle_comb1" fcomb;

  make_function modl "sim_reset" void [||] (compile_reset g_reg regs) |> ignore;

  (* dump_module modl; *)
  modl

(*let compile' = compile_simple*)
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
    sim_cycle_check = (fun () -> ());
    sim_cycle_comb0;
    sim_cycle_seq;
    sim_cycle_comb1;
    sim_reset;
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
    sim_cycle_check = (fun () -> ());
    sim_cycle_comb0;
    sim_cycle_seq;
    sim_cycle_comb1;
    sim_reset;
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
    let update_out_ports () = 
      List.iter (fun (s,(t,b,w)) -> t := B.of_bani_ptr w b !t) out_ports
    in

    let sim_cycle_check = sim.sim_cycle_check in
    let sim_cycle_comb0 () = update_in_ports (); sim.sim_cycle_comb0 (); update_out_ports () in
    let sim_cycle_seq = sim.sim_cycle_seq in
    let sim_cycle_comb1 () = sim.sim_cycle_comb1(); update_out_ports () in
    let sim_reset () = (*update_in_ports ();*) sim.sim_reset (); (*update_out_ports ()*) in

    {
      sim_internal_ports = [];
      sim_in_ports = List.map (fun (s,(t,b,w)) -> s,t) in_ports;
      sim_out_ports = List.map (fun (s,(t,b,w)) -> s,t) out_ports;
      sim_cycle_check;
      sim_cycle_comb0;
      sim_cycle_seq;
      sim_cycle_comb1;
      sim_reset;
    }

  let make circuit = wrap (make circuit)

  let write path circuit = write path circuit

  let load path = wrap (load path)

end

(* XXX delete me *)
(*
let test_bb () = 

  let context = global_context () in
  let modl = Llvm.create_module context "mytestmodl" in


  let open Llvm in

  let hmm = declare_function "hmm" (function_type void [||]) modl in

  let myfn fn builder =
    let body_blk = append_block (global_context()) "body" fn in
    let jmp = build_br body_blk builder in
    let () = position_at_end body_blk builder in

    let global () = 
      let bbprev = insertion_block builder in
      let () = position_before nop builder in
      let x = define_global  "global_var" (const_int int8 0) modl in
      let x = build_load x "" builder in
      let () = position_at_end bbprev builder in
      x
    in

    let a = build_add (const_int int8 1) (const_int int8 2) "add" builder in
    let g0 = global () in
    let g1 = global () in
    let b = build_add g0 g1 "add" builder in
    let g2 = global () in
    let c = build_add a b "add" builder in
    let d = build_add g2 c "add" builder in
    Llvm.build_ret d builder |> ignore;
    Printf.printf "ok %b\n" (Llvm_analysis.verify_function fn);
    fn
  in

  let fn = make_function modl "mytestfn" int8 [||] myfn in
  ignore @@ Llvm_bitwriter.write_bitcode_file modl "mytestfn.bc";
  Llvm_analysis.assert_valid_function fn;
  ()
*) 

