open HardCaml
module Sc = Signal.Comb
module St = Signal.Types

type cyclesim = Bits.Ext.Comb.BigarraybitsNativeint.t Cyclesim.Api.cyclesim

val reset_value : St.signal -> Llvm.llvalue

val compile : int -> Circuit.t -> Llvm.llmodule

val make : Circuit.t -> (Bits.Ext.Utils_ext.bani * int) Cyclesim.Api.cyclesim

val write : string -> Circuit.t -> unit

val load :
  string ->
  (Bits.Ext.Utils_ext.bani * int) Cyclesim.Api.cyclesim

module Make(B : Bits.Ext.Comb.S) : 
sig
  type t = B.t
  type base_cyclesim = cyclesim
  type cyclesim = t Cyclesim.Api.cyclesim
  val wrap :
    (Bits.Ext.Utils_ext.bani * int)
    Cyclesim.Api.cyclesim -> B.t Cyclesim.Api.cyclesim
  val make : Circuit.t -> B.t Cyclesim.Api.cyclesim
  val write : string -> Circuit.t -> unit
  val load : string -> B.t Cyclesim.Api.cyclesim
end

module Gen(B : Bits.Ext.Comb.S)(I : Interface.S) (O : Interface.S) : sig
  val make : string -> (Signal.Comb.t I.t -> Signal.Comb.t O.t) ->
    (Circuit.t * B.t Cyclesim.Api.cyclesim * B.t ref I.t * B.t ref O.t)
end

