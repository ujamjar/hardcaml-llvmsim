open Llvm
open Utils
open Globals
module Sc = HardCaml.Signal.Comb
module St = HardCaml.Signal.Types
module Ee = Llvm_executionengine.ExecutionEngine
module Gv = Llvm_executionengine.GenericValue

let _ = ignore (Llvm_executionengine.initialize_native_target ())

let compile_info modl name (ret,retz) args f values = 
  make_function modl name ret args
    (fun fn ->
       let arg = (params fn.func).(0) in
       let comparer (d,i) v = 
         let cmp = build_icmp Icmp.Eq (const_int 32 i) arg "cmp" fn.builder in 
         let s = build_select cmp (f fn.func fn.builder v) d "sel" fn.builder in
         (s, i+1)
       in
       let d,_ = List.fold_left comparer (retz, 0) values in
       build_ret d fn.builder
    ) |> ignore


(* get width of an IO signal *)
let compile_width modl io signals = 
  compile_info modl ("width_" ^ io) (int32,zero32) [|int32|]
    (fun fn builder signal -> const_int 32 (Sc.width signal))
    signals

(* get name of an IO signal.  Note, we generate a function which returns
 * each character in turn, rather than a pointer to the string *)
let compile_name modl io signals = 
  (* generate globals for the names *)
  let mk_global_name signal  = 
    let name = List.hd (St.names signal) in
    define_global ("name" ^ "_" ^ io ^ "_" ^ name) (const_string name) modl
  in
  let names = List.map mk_global_name signals in
  (* create look up function *)
  compile_info modl ("name_" ^ io) (int8,zero8) [|int32;int32|]
    (fun fn builder name -> 
       let name_gep builder g i = build_gep g [| zero32; i |] "gep" builder in
       build_load (name_gep builder name (params fn).(1)) "" builder
    ) names

(* returns a pointer to the data as a 'nativeint' which can then be wrapped
 * in a big array *)
let compile_ptr get_global modl io signals =
  let globals = List.map (fun s -> (get_global (Sc.width s) (St.uid s)).cur) signals in
  compile_info modl ("ptr_" ^ io) (int64,zero64) [|int32|]
    (fun fn builder d ->
       let bcast = build_bitcast d (ptr_type 1 (int64)) "bitcast" builder in
       let gep = build_gep bcast [| zero64 |] "gep" builder in
       let p2i = build_ptrtoint gep (int64) "get_p2i" builder in
       p2i
    ) globals

(* converts a nativeint (as queried from the simulation module) to a
 * bigarray *)
external llvm_get_ptr : nativeint -> int -> HardCaml.Bits_ext.Utils_ext.bani =
  "llvmsim_get_ptr"

(* find a function in the JIT, or raise an exception *)
let lookup_function name jit = 
  match Ee.find_function name jit with
  | Some(x) -> x
  | None -> failwith ("Couldn't find function " ^ name ^ " in JIT")

(* look up the circuit ports *)
let query_ports io jit =
  (* find the functions in the module *)
  let width = lookup_function ("width_" ^ io) jit in
  let name = lookup_function ("name_" ^ io) jit in
  let ptr = lookup_function ("ptr_" ^ io) jit in
  (* wrap so they can be called *)
  let width n = Gv.as_int (Ee.run_function width [| Gv.of_int int32 n |] jit) in
  let name n m = Gv.as_int (Ee.run_function name 
                              [| Gv.of_int int32 n; Gv.of_int int32 m |] jit) in
  let name n = 
    let rec b n m str =
      let x = name n m in
      if x <> 0 then b n (m+1) (str ^ Char.escaped (Char.chr x))
      else str
    in
    b n 0 ""
  in
  let ptr n = Gv.as_nativeint (Ee.run_function ptr [| Gv.of_int int32 n |] jit) in
  (* look up each port *)
  let rec lookup n = 
    let width,name,ptr = width n, name n, ptr n in
    if name = "" then []
    else (name, ref (llvm_get_ptr ptr width, width)) :: lookup (n+1)
  in
  List.rev (lookup 0)

