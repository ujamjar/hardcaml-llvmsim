open HardCaml.Signal.Types
module Sc = HardCaml.Signal.Comb

val compile_comb :
  Llvm.llmodule -> Llvm.llvalue -> Llvm.llbuilder ->
  ('a -> signal -> Llvm.llvalue) ->
  'a -> signal -> Llvm.llvalue

val compile_comb_list :
  Llvm.llmodule -> Llvm.llvalue -> Llvm.llbuilder ->
  (Llvm.llvalue UidMap.t ->
   signal -> Llvm.llvalue) ->
  Llvm.llvalue UidMap.t ->
  signal list -> Llvm.llvalue UidMap.t

val compile_reg :
  Llvm.llbuilder ->
  (signal -> Llvm.llvalue) ->
  (Llvm.llvalue -> signal -> 'a) -> signal -> 'a

val compile_mem :
  Llvm.llbuilder ->
  (signal -> Llvm.llvalue) ->
  (Llvm.llvalue -> signal -> 'a) -> signal -> unit


