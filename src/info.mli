module Sc = HardCaml.Signal.Comb
module St = HardCaml.Signal.Types
module Ee = Llvm_executionengine

val compile_info :
  Llvm.llmodule -> string ->
  Llvm.lltype * Llvm.llvalue -> Llvm.lltype array ->
  (Llvm.llvalue -> Llvm.llbuilder -> 'a -> Llvm.llvalue) -> 'a list -> unit

val compile_width : Llvm.llmodule -> string -> Sc.t list -> unit

val compile_name : Llvm.llmodule -> string -> St.signal list -> unit

val compile_ptr : (int -> St.signal -> Globals.global) -> Llvm.llmodule -> string -> Sc.t list -> unit

val query_ports : string ->
  Ee.llexecutionengine -> (string * (HardCaml.Bits.Ext.Utils_ext.bani * int) ref) list
