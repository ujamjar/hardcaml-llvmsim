module Sc = HardCaml.Signal.Comb
module St = HardCaml.Signal.Types
module Ee = Llvm_executionengine.ExecutionEngine

val compile_info :
  Llvm.llmodule -> string ->
  Llvm.lltype * Llvm.llvalue -> Llvm.lltype array ->
  (Llvm.llvalue -> Llvm.llbuilder -> 'a -> Llvm.llvalue) -> 'a list -> unit

val compile_width : Llvm.llmodule -> string -> Sc.t list -> unit

val compile_name : Llvm.llmodule -> string -> St.signal list -> unit

val compile_ptr : (int -> St.uid -> Globals.global) -> Llvm.llmodule -> string -> Sc.t list -> unit

external llvm_get_ptr : nativeint -> int -> HardCaml.Bits_ext.Utils_ext.bani = "llvmsim_get_ptr"

val lookup_function : string -> Ee.t -> Llvm.llvalue

val query_ports : string ->
  Ee.t -> (string * (HardCaml.Bits_ext.Utils_ext.bani * int) ref) list
