open Llvm
open Utils
open Globals
open HardCaml.Signal.Types
module Sc = HardCaml.Signal.Comb

let compile_comb modl gfn fn builder map signal =
  let sdep n = List.nth (deps signal) n in
  let dep map i = load_signal gfn map (sdep i) in
  let name n = name n signal in

  let rec load_list map r = function
    | [] -> map, List.rev r
    | h::t -> 
      let map,instr = load_signal gfn map h in
      load_list map (instr::r) t
  in
  let add_signal v map = UidMap.add (uid signal) v map, v in

  let compile_cat () =
    let name = name "cat" in
    let width = Sc.width signal in
    let ze s w = build_zext s (int_type w) name builder in
    let or2 r s n =
      if n = 0 then
        ze s width
      else
        let f s n = build_shl (ze s width) 
            (const_int width n) 
            "cat_shl" builder 
        in
        build_or r (f s n) name builder
    in
    let sdeps = deps signal in
    let map, deps = load_list map [] sdeps in
    let d = List.rev (HardCaml.Utils.map2 (fun s d -> Sc.width s, d) sdeps deps) in
    let r = 
      fst(List.fold_left 
        (fun (res,sft) (wid,arg) -> or2 res arg sft,sft+wid)
        (const_int width 0, 0) d)
    in
    add_signal r map
  in

  let compile_select h l = 
    let name = name "select" in
    let map,d = dep map 0 in
    let w = Sc.width (sdep 0) in
    if w = (h-l+1) then add_signal d map
    else
      let sft = const_int w l in
      let s = build_lshr d sft name builder in
      add_signal (build_trunc s (int_type (h-l+1)) name builder) map
  in

  let compile_mul signed id = 
    let name = name "mul" in
    let ext = if signed then build_sext else build_zext in
    let map, d0 = dep map 0 in
    let map, d1 = dep map 1 in
    let a = ext d0 (int_type id.s_width) name builder in
    let b = ext d1 (int_type id.s_width) name builder in
    add_signal (build_mul a b name builder) map (* XXX this may only work upto a max no of bits *)
  in

  (* build a select table and jump targets for the mux *)
  let compile_generic_mux () = 
    let name = name "gmux" in
    let entry_bb () = builder_at 
        (global_context()) (instr_begin (entry_block fn)) in
    let alloca w = build_alloca (int_type w) name (entry_bb ()) in 
    let r = alloca (Sc.width signal) in
    let sel_bb = insertion_block builder in 
    let case_bbs = 
      (* for some reason I dont get, the cases come out
       * backwards in the llvm code *)
      let append_blk _ = 
        let bb = append_block (name^"_bb") fn in
        position_at_end bb builder;
        bb
      in
      List.map append_blk (List.tl (deps signal))
    in
    let end_bb = append_block  (name^"_bb_end") fn in
    let map,_ = List.fold_left (fun (map,i) bb ->
        position_at_end bb builder;
        let map, dep = dep map (i+1) in
        build_store dep r builder |> ignore;
        build_br end_bb builder |> ignore;
        map, i+1
      ) (map,0) case_bbs in
    position_at_end sel_bb builder;
    let map, dep = dep map 0 in
    make_switch (Sc.width signal) dep case_bbs builder;
    position_at_end end_bb builder;
    add_signal (build_load r name builder) map
  in

  (* a simple mux is one with only a few cases, and is implemented as
   * a chain of 'select' statements.  Not actually sure this is
   * worthwhile. *)
  let compile_simple_mux () = 
    let name = name "smux" in
    let wsel = Sc.width (List.hd (deps signal)) in
    let map, deps = load_list map [] (deps signal) in
    let sel,cases = List.hd deps, List.tl deps in
    let def,cases =
      let c = List.rev cases in
      List.hd c, List.rev (List.tl c)
    in
    let r, i = List.fold_left (fun (r,i) v -> 
        build_select 
          (build_icmp Icmp.Eq sel (const_int wsel i) name builder) 
          v r name builder, 
        (i+1)
      ) (def,0) cases in
    add_signal r map
  in

  (* a constant mux has only constants as it's cases and is built
   * using a global array *)
  let is_constant_mux () = 
    List.fold_left (fun b s -> is_const s && b)
      true (List.tl (deps signal))
  in
  let compile_constant_mux () = 
    let name = name "cmux" in
    let size = List.length (List.tl (deps signal)) in
    let global = lookup_global name modl in
    let global = 
      match global with
      | Some (x) -> x
      | None ->
        (* create initialized array *)
        let values = 
          List.map (fun s -> const_of_signal s) (List.tl (deps signal))
          |> Array.of_list
        in
        let values = const_array (int_type (Sc.width signal)) values in
        let g = define_global name values modl in
        set_linkage Linkage.Internal g;
        g
    in
    let map, sel = dep map 0 in
    let w = Sc.width (sdep 0) in
    let max = const_int (w+1) (size-1) in
    let sel = build_uresize sel w (w+1) name builder in
    let sel = 
      build_select 
        (build_icmp Icmp.Ule sel max name builder)
        sel max name builder
    in
    let addr = build_gep global [| zero32; sel |] name builder in
    add_signal (build_load addr name builder) map
  in

  (* select the mux implementation *)
  let compile_mux () = 
    if List.length (deps signal) <= (4+1) then
      compile_simple_mux ()
    else if is_constant_mux () then
      compile_constant_mux ()
    else 
      compile_generic_mux ()
  in

  let compile_mem () = 
    let map, rmem = load_signal gfn map signal in
    add_signal rmem map
  in

  let compile_bop f name = 
    let map, d0 = dep map 0 in
    let map, d1 = dep map 1 in
    add_signal (f d0 d1 name builder) map
  in
  let compile_uop f name = 
    let map, d0 = dep map 0 in
    add_signal (f d0 name builder) map
  in

  (* compile each type of signal *)
  match signal with
  | Signal_empty -> failwith "cant compile empty signal"
  | Signal_const(_,v) -> failwith "cant compile constants"
  | Signal_op(id,op) ->
    begin
      match op with
      | Signal_add -> compile_bop build_add (name "add") 
      | Signal_sub -> compile_bop build_sub (name "sub") 
      | Signal_mulu -> compile_mul false id
      | Signal_muls -> compile_mul true id
      | Signal_and -> compile_bop build_and (name "and") 
      | Signal_or -> compile_bop build_or (name "or") 
      | Signal_xor -> compile_bop build_xor (name "xor") 
      | Signal_eq -> compile_bop (build_icmp Icmp.Eq) (name "eq") 
      | Signal_not -> compile_uop build_not (name "not") 
      | Signal_lt -> compile_bop (build_icmp Icmp.Ult) (name "lt") 
      | Signal_cat -> compile_cat ()
      | Signal_mux -> compile_mux ()
    end
  | Signal_wire(_) -> let map, r = dep map 0 in add_signal r map
  | Signal_select(_,h,l) -> compile_select h l
  | Signal_reg(_,r) -> failwith "Registers not expected here"
  | Signal_mem(_,_,r,m) -> compile_mem ()
  | Signal_inst(_) -> failwith "Instantiations are not supported in simulation"

let compile_comb_list modl gfn fn builder map signals = 
  let compile_comb = compile_comb modl gfn fn builder in
  List.fold_left (fun map s -> fst (compile_comb map s)) map signals

(* let compile_reg *)
let compile_reg builder load store signal = 
  let r = 
    match signal with 
    | Signal_reg(_,r) -> r 
    | _ -> failwith "compile_reg: not a register!" 
  in
  let name n = name n signal in
  let width = Sc.width signal in
  let src = load (List.hd (deps signal)) in (* input data *)
  let cur_q = load signal in (* current value *)
  (* clear logic *)
  let clr, clr_level, clr_value = (* clear and enable logic *)
    if r.reg_clear <> Sc.empty then
      load r.reg_clear, load r.reg_clear_level, load r.reg_clear_value
    else 
      const_int 1 0, const_int 1 1, const_int width 0
  in
  let clr = build_icmp Icmp.Eq clr clr_level (name "reg_clr") builder in
  (* enable logic *)
  let ena = if r.reg_enable <> Sc.empty then load r.reg_enable else const_int 1 1 in
  let ena = build_and ena (build_not clr (name "reg_clr_not") builder) (name "reg_ena") builder in
  (* output *)
  let q = build_select clr clr_value cur_q (name "reg_clr_update") builder in
  let q = build_select ena src q (name "reg_ena_update") builder in
  store q signal

(* let compile_mem *)
let compile_mem builder load store signal = 
  let r,m = 
    match signal with 
    | Signal_mem(_,_,r,m) -> r,m 
    | _ -> failwith "compile_mem: not a memory!" 
  in
  let name n = name n signal in
  let src = load (List.hd (deps signal)) in (* input data *)
  let cur_q = load signal in (* current value at address *)
  let ena = load r.reg_enable in (* write enable *)
  let q = build_select ena src cur_q (name "mem_ena_update") builder in
  store q signal



