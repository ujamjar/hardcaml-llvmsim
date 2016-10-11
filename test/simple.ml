module I = interface a[8] b[8] end
module O = interface c[9] end
let f i = O.{ c = I.(Comb.(ue i.a +: ue i.b)) }

module B = HardCaml.Bits.Ext.Comb.BigarraybitsNativeint
module X = HardCamlLlvmsim.Sim.Gen(B)(I)(O);;

module S = HardCaml.Cyclesim.Api
let _,sim,i,o = X.make "foo" f

open I
open O

let () = 
  for j=0 to 10 do
    i.a := B.srand 8;
    i.b := B.srand 8;
    S.cycle sim;
    Printf.printf "%i + %i = %i\n"
      B.(to_int !(i.a))
      B.(to_int !(i.b))
      B.(to_int !(o.c))
  done

