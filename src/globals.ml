open Llvm
open HardCaml.Signal.Types
open Utils
module Sc = HardCaml.Signal.Comb


(* Dynamic generation of globals 
 *
 * (1) Input/Output Signals (IOs)
 * (2) Registers (REGs)
 * (3) Memories  (MEMs)
 * (4) Temporaries between sub-cycles (TEMPs)
 * (5) Internally monitored signals
 *
 * Globals have different types.  
 *
 * IOs - statically created 'n' bit signals
 * REGs - 2 element arrays (cur and next values)
 * MEMs - 'n' element arrays + next value
 * TEMPs - dynamically stored 'n' bit signals
 *
 * NOTE: With output signals we must write them out.  We can do so as we
 * calculate them (in particular because they are always wires).  To 
 * support wires we could do much the same thing - effectively create 
 * output wires for them.  Hmm interesting.
 *
*)

(* one of the global value types *)

type global_type = G_Port | G_Internal | G_Reg | G_Mem
type global = 
  {
    width : int;
    rnd_width : int;
    cur : llvalue;
    next : llvalue;
    typ : global_type;
  }

(* some type abbreviations *)
type global_simple = bool -> int -> uid -> global
type global_reg = int -> uid -> global
type global_mem = int -> int -> uid -> global
type global_fns = global_simple * global_reg * global_mem

type load_simple = bool -> signal -> llvalue
type load_reg = signal -> llvalue
type load_mem = llvalue -> signal -> llvalue
type load_fns = load_simple * load_reg * load_mem

type store_simple = llvalue -> bool -> signal -> unit
type store_reg = llvalue -> signal -> unit
type store_mem = llvalue -> signal -> unit
type store_fns = store_simple * store_reg * store_mem

let globals modl = 
  let simple port width uid = 
    let name = (if port then "port_" else "internal_") ^ 
               string_of_int width ^ "_" ^ Int64.to_string uid in
    let width' = if port then pbits width else width in
    let cur = define_global name (const_int width' 0) modl in
    set_linkage Linkage.Internal cur;
    let g = {
      width = width;
      rnd_width = width';
      cur = cur;
      next = cur;
      typ = if port then G_Port else G_Internal;
    } in
    g
  in
  let reg width uid = 
    let name = "reg_" ^ string_of_int width ^ "_" ^ Int64.to_string uid in
    let cur = define_global name (const_int width 0) modl in
    let next = define_global (name^"_next") (const_int width 0) modl in
    set_linkage Linkage.Internal cur;
    set_linkage Linkage.Internal next;
    let g = {
      width = width;
      rnd_width = width;
      cur = cur;
      next = next;
      typ = G_Reg;
    } in
    g
  in
  let mem width count uid = 
    let name = "mem_" ^ string_of_int width ^ "_" ^ 
               string_of_int count ^ "_" ^ Int64.to_string uid in
    let cur = declare_global (array_type (int_type width) count) name modl in
    let next = define_global (name^"_next") (const_int width 0) modl in
    set_linkage Linkage.Internal cur;
    set_linkage Linkage.Internal next;
    let g = {
      width = width;
      rnd_width = width;
      cur = cur;
      next = next;
      typ = G_Mem;
    } in
    g
  in
  let globals = ref UidMap.empty in
  let memoize f uid = 
    try UidMap.find uid !globals
    with _ -> 
      let g = f uid in
      globals := UidMap.add uid g !globals;
      g
  in
  let simple r w = memoize (simple r w) in
  let reg w = memoize (reg w) in
  let mem w c = memoize (mem w c) in
  globals, (simple, reg, mem)


let load fn (simple,reg,mem) = 
  let entry_bb () = builder_at (global_context()) 
      (instr_begin (entry_block fn)) in
  let load_simple port s = 
    let builder = entry_bb () in
    let name = name "sload" s in
    let g = simple port (Sc.width s) (uid s) in 
    let x = build_load g.cur name builder in
    if g.width = g.rnd_width then x
    else build_trunc x (int_type g.width) name builder
  in
  let load_reg s = 
    let builder = entry_bb () in
    let name = name "rload" s in
    let g = reg (Sc.width s) (uid s) in 
    let x = build_load g.cur name builder in
    build_uresize x g.rnd_width g.width name builder
  in
  let load_mem addr s = 
    let builder = entry_bb () in
    let name = name "mload" s in
    let g = mem (Sc.width s) (memsize s) (uid s) in
    let addr = build_gep g.cur [| zero32; addr |] "" builder in
    build_load addr name builder
  in
  let globals = ref UidMap.empty in
  let memoize f s = 
    try UidMap.find (uid s) !globals
    with _ ->
      let instr = f s in 
      globals := UidMap.add (uid s) instr !globals;
      instr
  in
  let load_simple port = memoize (load_simple port) in
  let load_reg = memoize load_reg in
  let load_mem addr = memoize (load_mem addr) in
  load_simple, load_reg, load_mem

let store builder (simple,reg,mem) = 
  let store_simple instr port s = 
    let name = name "sstore" s in
    let g = simple port (Sc.width s) (uid s) in
    let x = build_uresize instr g.width g.rnd_width name builder in
    build_store x g.next builder |> ignore
  in
  let store_reg instr s = 
    let g = reg (Sc.width s) (uid s) in
    build_store instr g.next builder |> ignore
  in
  let store_mem instr s = 
    let g = mem (Sc.width s) (memsize s) (uid s) in
    build_store instr g.next builder |> ignore
  in
  store_simple, store_reg, store_mem

type update_reg = signal -> unit
type update_mem = llvalue -> signal -> unit
type update_fns = update_reg * update_mem

let update builder (simple,reg,mem) = 
  let update_reg s =
    let g = reg (Sc.width s) (uid s) in
    let x = build_load g.next "" builder in
    build_store x g.cur builder |> ignore
  in 
  let update_mem addr s = 
    let g = mem (Sc.width s) (memsize s) (uid s) in
    let addr = build_gep g.cur [| zero32; addr |] "" builder in
    let x = build_load g.next "" builder in
    build_store x addr builder |> ignore
  in
  update_reg, update_mem

let rec load_signal (s,r,m) map signal = 
  match signal with
  | Signal_const(_) -> const_of_signal signal
  | _ ->
    begin
      try UidMap.find (uid signal) map
      with _ ->
        begin
          match signal with
          | Signal_reg(_) -> r signal
          | Signal_mem(_,_,_,x) -> 
            m (load_signal (s,r,m) map x.mem_read_address) signal
          | _ -> s false signal
        end
    end
