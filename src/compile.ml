open Llvm
open Utils
open Globals
open HardCaml.Signal.Types
module Sc = HardCaml.Signal.Comb

let compile_comb modl fn builder load map signal =
  let sdep n = List.nth (deps signal) n in
  let instr = load map in
  let dep = instr << sdep in
  let name n = name n signal in

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
    let deps = HardCaml.Utils.mapi (fun i _ -> dep i) sdeps in
    let d = List.rev (HardCaml.Utils.map2 (fun s d -> Sc.width s, d) sdeps deps) in
    fst(List.fold_left 
          (fun (res,sft) (wid,arg) -> or2 res arg sft,sft+wid)
          (const_int width 0, 0) d)
  in

  let compile_select h l = 
    let name = name "select" in
    let d = dep 0 in
    let w = Sc.width (sdep 0) in
    if w = (h-l+1) then d
    else
      let sft = const_int w l in
      let s = build_lshr d sft name builder in
      build_trunc s (int_type (h-l+1)) name builder
  in

  let compile_mul signed id = 
    let name = name "mul" in
    let ext = if signed then build_sext else build_zext in
    let a = ext (dep 0) (int_type id.s_width) name builder in
    let b = ext (dep 1) (int_type id.s_width) name builder in
    build_mul a b name builder (* XXX this may only work upto a max no of bits *)
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
    HardCaml.Utils.iteri (fun i bb ->
        position_at_end bb builder;
        build_store (dep (i+1)) r builder |> ignore;
        build_br end_bb builder |> ignore
      ) case_bbs;
    position_at_end sel_bb builder;
    make_switch (Sc.width signal) (dep 0) case_bbs builder;
    position_at_end end_bb builder;
    build_load r name builder
  in

  (* a simple mux is one with only a few cases, and is implemented as
   * a chain of 'select' statements.  Not actually sure this is
   * worthwhile. *)
  let compile_simple_mux () = 
    let name = name "smux" in
    let wsel = Sc.width (List.hd (deps signal)) in
    let deps = List.map instr (deps signal) in
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
    r
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
    let sel = dep 0 in
    let w = Sc.width (sdep 0) in
    let max = const_int (w+1) (size-1) in
    let sel = build_uresize sel w (w+1) name builder in
    let sel = 
      build_select 
        (build_icmp Icmp.Ule sel max name builder)
        sel max name builder
    in
    let addr = build_gep global [| zero32; sel |] name builder in
    build_load addr name builder
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

  (* compile each type of signal *)
  match signal with
  | Signal_empty -> failwith "cant compile empty signal"
  | Signal_const(_,v) -> failwith "cant compile constants"
  | Signal_op(id,op) ->
    begin
      match op with
      | Signal_add -> build_add (dep 0) (dep 1) (name "add") builder  
      | Signal_sub -> build_sub (dep 0) (dep 1) (name "sub") builder
      | Signal_mulu -> compile_mul false id
      | Signal_muls -> compile_mul true id
      | Signal_and -> build_and (dep 0) (dep 1) (name "and") builder 
      | Signal_or -> build_or (dep 0) (dep 1) (name "or") builder
      | Signal_xor -> build_xor (dep 0) (dep 1) (name "xor") builder
      | Signal_eq -> build_icmp Icmp.Eq (dep 0) (dep 1) (name "eq") builder
      | Signal_not -> build_not (dep 0) (name "not") builder
      | Signal_lt -> build_icmp Icmp.Ult (dep 0) (dep 1) (name "lt") builder
      | Signal_cat -> compile_cat ()
      | Signal_mux -> compile_mux ()
    end
  | Signal_wire(_) -> dep 0
  | Signal_select(_,h,l) -> compile_select h l
  | Signal_reg(_,r) -> failwith "Registers not expected here"
  | Signal_mem(_,_,r,m) -> failwith "Memories not expected here"
  | Signal_inst(_) -> failwith "Instantiations are not supported in simulation"

let compile_comb_list modl fn builder load map signals = 
  let compile_comb = compile_comb modl fn builder load in
  List.fold_left (fun map s ->
      UidMap.add (uid s) (compile_comb map s) map)
    map signals

(* let compile_reg *)
let compile_reg builder load store signal = 
  let r = match signal with Signal_reg(_,r) -> r | _ -> failwith "" in
  let sdep n = List.nth (deps signal) n in
  let instr = load in
  let dep = instr << sdep in
  let name n = name n signal in
  let width = Sc.width signal in
  let src = dep 0 in (* input data *)
  (*let cur_q = UidMap.find (uid signal) reg_globals_loaded in*)
  let cur_q = instr signal in
  let clr, clr_level, clr_value = 
    if r.reg_clear <> Sc.empty then
      instr r.reg_clear, instr r.reg_clear_level, instr r.reg_clear_value
    else 
      const_int 1 0, const_int 1 1, const_int width 0
  in
  let clr = build_icmp Icmp.Eq clr clr_level (name "reg_clr") builder in
  let ena = 
    if r.reg_enable <> Sc.empty then instr r.reg_enable
    else const_int 1 1
  in
  let ena = build_and ena 
      (build_not clr (name "reg_clr_not") builder) 
      (name "reg_ena") builder in
  let q = build_select clr clr_value cur_q 
      (name "reg_clr_update") builder in
  let q = build_select ena src q 
      (name "reg_ena_update") builder in
  (*let (d,w,w') = UidMap.find (uid signal) l.reg_globals in*)
  store q signal

(* let compile_mem *)



