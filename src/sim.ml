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

let compile_simple circuit = 
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

  let compile_cycle cycle builder = 
    let l_ops = Globals.load cycle g_ops in
    let load = Globals.load_signal l_ops in
    let s_simple,s_reg,_ = Globals.store builder g_ops in
    let u_reg,_ = Globals.update builder g_ops in
    let map = Compile.compile_comb_list 
        modl cycle builder load
        UidMap.empty schedule 
    in
    let store rnd s = 
      try s_simple (load map s) rnd s 
      with _ -> ()
    in
    let store_reg instr s = s_reg instr s in
    let update_reg s = u_reg s in
    let return () = Llvm.build_ret_void builder |> ignore in
    let compile_reg = Compile.compile_reg builder (load map) store_reg in
    (* store outputs *)
    List.iter (store true) (Circuit.outputs circuit);
    (* sequential logic *)
    List.iter compile_reg regs;
    List.iter update_reg regs;
    (* return value *)
    return();
    Llvm_analysis.assert_valid_function cycle |> ignore
  in

  let compile_reset reset builder = 
    List.iter (fun s ->
        let v = reset_value s in
        let g = g_reg (Sc.width s) (uid s) in
        ignore (Llvm.build_store v g.Globals.cur builder) 
      ) regs;
    Llvm.build_ret_void builder |> ignore;
    Llvm_analysis.assert_valid_function reset |> ignore
  in

  make_function modl "reset" void [||] compile_reset;
  make_function modl "cycle" void [||] compile_cycle;

  (* dump_module modl;*)
  modl

let compile max circuit = 

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

  let compile_cycle cycle builder = 

    let compile_cycle signals cycle builder = 
      Llvm.set_linkage Llvm.Linkage.Internal cycle;
      let l_ops = Globals.load cycle g_ops in
      let load = Globals.load_signal l_ops in
      let s_simple,s_reg,_ = Globals.store builder g_ops in
      let map = Compile.compile_comb_list 
          modl cycle builder load
          UidMap.empty signals
      in
      let store rnd s = 
        try s_simple (load map s) rnd s 
        with _ -> ()
      in
      let return () = Llvm.build_ret_void builder |> ignore in
      cycle, map, store, return 
    in

    let compile_reg_store signals cycle builder = 
      Llvm.set_linkage Llvm.Linkage.Internal cycle;
      let l_ops = Globals.load cycle g_ops in
      let load = Globals.load_signal l_ops in
      let _,s_reg,_ = Globals.store builder g_ops in
      let compile_reg = Compile.compile_reg builder 
          (load UidMap.empty) s_reg in
      List.iter compile_reg signals;
      Llvm.build_ret_void builder |> ignore;
      cycle
    in

    let compile_reg_update signals cycle builder = 
      Llvm.set_linkage Llvm.Linkage.Internal cycle;
      let u_reg,_ = Globals.update builder g_ops in
      List.iter u_reg signals;
      Llvm.build_ret_void builder |> ignore;
      cycle
    in

    let rec build name n f signals = 
      let rec split n a b = 
        if n=max then List.rev a, b
        else
          match b with
          | [] -> List.rev a,[]
          | h::t -> split (n+1) (h::a) t
      in
      let split = split 0 [] in
      let h,t = split signals in
      let r = 
        make_function modl 
          (name ^ "_" ^ string_of_int n) void [||]
          (f h)
      in
      if t=[] then [r]
      else r :: build name (n+1) f t
    in

    (* build the sub-cycle functions *)
    let r = build "cycle" 0 compile_cycle schedule in

    (* build register updates *)
    let reg_store = build "reg_store" 0 compile_reg_store regs in
    let reg_update = build "reg_update" 0 compile_reg_update regs in

    (* sort out inter-sub-cycle dependancies *)
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
    List.iter (fun (_,_,_,r) -> r()) r;

    (* call sub-cycle functions *)
    List.iter (fun (f,_,_,_) -> Llvm.build_call f [||] "" builder |> ignore) r;
    (* call reg update function *)
    List.iter (fun f -> Llvm.build_call f [||] "" builder |> ignore) reg_store; 
    List.iter (fun f -> Llvm.build_call f [||] "" builder |> ignore) reg_update;
    Llvm.build_ret_void builder |> ignore 
  in

  let compile_reset reset builder = 
    List.iter (fun s ->
        let v = reset_value s in
        let g = g_reg (Sc.width s) (uid s) in
        ignore (Llvm.build_store v g.Globals.cur builder) 
      ) regs;
    Llvm.build_ret_void builder |> ignore;
    Llvm_analysis.assert_valid_function reset |> ignore
  in

  make_function modl "reset" void [||] compile_reset;
  make_function modl "cycle" void [||] compile_cycle;

  (* dump_module modl; *)
  modl


let make circuit = 
  let modl = compile 100 circuit in
  (*let mp = ModuleProvider.create modl in
    let jit = Ee.create_jit mp in*)
  let optlevel = 2 in
  let jit = Ee.create_jit modl optlevel in
  let mk name = 
    let f = Info.lookup_function name jit in
    (fun () -> Ee.run_function f [||] jit |> ignore) 
  in
  let cycle = mk "cycle" in 
  let reset = mk "reset" in
  let in_ports = Info.query_ports "i" jit in
  let out_ports = Info.query_ports "o" jit in
  {
    sim_cycle_comb0 = cycle;
    sim_cycle_check = (fun () -> ());
    sim_cycle_comb1 = (fun () -> ());
    sim_cycle_seq = (fun () -> ());
    sim_reset = reset;
    sim_internal_ports = [];
    sim_in_ports = in_ports;
    sim_out_ports = out_ports;
  }

let write path circuit = 
  let modl = compile 100 circuit in
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
  let cycle = Info.lookup_function ("cycle_" ^ name) jit in
  let reset = Info.lookup_function ("reset_" ^ name) jit in

  let in_ports = Info.query_ports "i" jit in
  let out_ports = Info.query_ports "o" jit in

  let cycle = (fun () -> ignore (Ee.run_function cycle [||] jit)) in
  let reset = (fun () -> ignore (Ee.run_function reset [||] jit)) in

  {
    sim_cycle_comb0 = cycle;
    sim_cycle_check = (fun () -> ());
    sim_cycle_comb1 = (fun () -> ());
    sim_cycle_seq = (fun () -> ());
    sim_reset = reset;
    sim_internal_ports = [];
    sim_in_ports = in_ports;
    sim_out_ports = out_ports;
  }

module Make(B : Bits_ext.S) =
struct

  type t = B.t
  type base_cyclesim = cyclesim
  type cyclesim = t Cyclesim.Api.cyclesim

  let wrap sim = 
    let in_ports = List.map (fun (s,d) -> 
        let d,w = fst !d, snd !d in s, (ref (B.zero w), d, w)) sim.sim_in_ports in
    let out_ports = List.map (fun (s,d) -> 
        let d,w = fst !d, snd !d in s, (ref (B.zero w), d, w)) sim.sim_out_ports in
    let cycle () = 
      List.iter (fun (s,(t,b,w)) -> B.to_bani_ptr !t b) in_ports; 
      sim.sim_cycle_comb0();
      List.iter (fun (s,(t,b,w)) -> t := B.of_bani_ptr w b !t) out_ports
    in
    let reset () = 
      List.iter (fun (s,(t,b,w)) -> B.to_bani_ptr !t b) in_ports;
      sim.sim_reset();
      List.iter (fun (s,(t,b,w)) -> t := B.of_bani_ptr w b !t) out_ports
    in
    {
      sim_cycle_comb0 = cycle;
      sim_cycle_check = (fun () -> ());
      sim_cycle_comb1 = (fun () -> ());
      sim_cycle_seq = (fun () -> ());
      sim_reset = reset;
      sim_internal_ports = [];
      sim_in_ports = List.map (fun (s,(t,b,w)) -> s,t) in_ports;
      sim_out_ports = List.map (fun (s,(t,b,w)) -> s,t) out_ports;
    }

  let make circuit = wrap (make circuit)

  let write path circuit = write path circuit

  let load path = wrap (load path)

end

