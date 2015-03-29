open HardCaml
open Llvm
module Sc = Signal.Comb
module St = Signal.Types

let (>>) f g x = g (f x)
let (<<) f g x = f (g x)
let (|>) x f = f x

let global_context =
  let gc = ref None in
  (fun () ->
    match !gc with
    | None ->
      let x = global_context() in
      gc := Some(x);
      x
    | Some(x) ->
      x)

let platform_bits = HardCaml.Utils.platform_bits
let pbits w = ((w + platform_bits) / platform_bits) * platform_bits 

(* types and constants *)
let int_type width = integer_type (global_context()) width
let const_int width value radix = 
  const_int_of_string (int_type width) value radix
let const_of_signal s = const_int (Sc.width s) (St.const_value s) 2 
let const_int width value = const_int width (string_of_int value) 10 
let const_string str = const_stringz (global_context()) str
let int64, int32, int8 = int_type 64, int_type 32, int_type 8
let zero64, zero32, zero8 = const_int 64 0, const_int 32 0, const_int 8 0
let rec ptr_type n t = 
  if n = 0 then t
  else ptr_type (n-1) (pointer_type t)
let void = void_type (global_context())

let build_uresize d width_from width_to name builder = 
  if width_from = width_to then d
  else if width_from < width_to then
    build_zext d (int_type width_to) name builder
  else
    build_trunc d (int_type width_to) name builder

(* switch *)
let make_switch width_sel' sel cases builder = 
  let def, cases = 
    let c = List.rev cases in
    List.hd c, List.rev (List.tl c)
  in
  let width_sel = max 32 (width_sel' + 1) in
  let sel = build_uresize sel width_sel' width_sel "switch" builder in 
  let switch = build_switch sel def (List.length cases) builder in
  HardCaml.Utils.iteri (fun i bb ->
      add_case switch (const_int width_sel i) bb)
    cases

(* builders *)
let builder() = builder (global_context())
let append_block name f = append_block (global_context()) name f

type 'a func =
  {
    func : llvalue;
    builder : llbuilder;
  }

(* build a function with the given argument and return types *)
let make_function modl name returns args f = 
  let builder = builder () in
  let fn_type = function_type returns args in
  let fn = declare_function name fn_type modl in
  let bb = append_block "entry" fn in
  position_at_end bb builder;
  f {func=fn; builder}

let name n s = 
  let names = String.concat "_" (St.names s) in
  n ^ "_" ^ names ^ "_" ^ Int64.to_string (St.uid s) ^ "_s"
let memsize = function St.Signal_mem(_,_,_,m) -> m.St.mem_size | _ -> 0 

(* select 1st 'n' elements from list *)
let split n l = 
  let rec split n a l =
    if n=0 then a, l
    else
      match l with
      | [] -> a, l
      | b::c -> split (n-1) (b::a) c
  in
  let a,b = split n [] l in
  List.rev a, b

let puts modl = 
  let ft = Llvm.function_type void [| ptr_type 1 int8 |] in
  let f = 
    match Llvm.lookup_function "puts" modl with
    | Some(f) -> f
    | None -> Llvm.declare_function "puts" ft modl 
  in
  (fun builder s ->
    let x = Llvm.define_global "debug" (Llvm.const_stringz (global_context()) s) modl in
    let x = Llvm.build_gep x [|zero32; zero32|] "" builder in
    Llvm.build_call f [|x|] "" builder)

let assert_valid_function fn = 
  if not (Llvm_analysis.verify_function fn) then begin
    Llvm.dump_value fn;
    Llvm_analysis.assert_valid_function fn;
    ()
  end


