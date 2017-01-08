(* test the memory implementation *)

open HardCaml
module B = Bits.Ext.Comb.IntbitsList
module S = Cyclesim.Api

module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
module Waveterm_ui = HardCamlWaveTerm.Ui.Make(B)(Waveterm_waves)

open HardCaml.Signal.Comb 
open HardCaml.Signal.Seq 

module type MemParams = sig
  val dbits : int 
  val abits : int 
end

module Mem(M : MemParams) = struct
  module I = struct
type 'a t = { d : 'a[@bits M.dbits]; r : 'a[@bits M.abits]; w : 'a[@bits M.abits]; we : 'a[@bits 1]; }[@@deriving hardcaml]
end
  module O = struct
type 'a t = { q : 'a[@bits M.dbits]; }[@@deriving hardcaml]
end
  let f i = O.({ q = I.(memory ~size:(1 lsl M.abits) ~spec:r_none ~we:i.we ~w:i.w ~d:i.d ~r:i.r) })
end

module Mem2(M : MemParams) = struct
  module X = Mem(M) 
  module I = struct
type 'a t = { r: 'a X.I.t; we2 : 'a[@bits 1]; }[@@deriving hardcaml]
end
  module O = struct
type 'a t = { q1 : 'a[@bits M.dbits]; q2 : 'a[@bits M.dbits]; }[@@deriving hardcaml]
end
  let f i = 
    let open X.I in
    let open I in
    let open O in
    let q1 = 
      I.(memory ~size:(1 lsl M.abits) ~spec:r_none ~we:i.r.we ~w:i.r.w ~d:i.r.d ~r:i.r.r) 
    in
    let q2 = 
      let r = sel_bottom q1 M.abits in
      I.(memory ~size:(1 lsl M.abits) ~spec:r_none ~we:i.we2 ~w:i.r.w ~d:i.r.d ~r) 
    in
    O.({ q1; q2 })

end

module X = struct
  let dbits = 8
  let abits = 4
end

module M = Mem2(X)

module N = HardCamlLlvmsim.Sim.Gen(B)(M.I)(M.O)
module N2 = HardCaml.Interface.Gen(B)(M.I)(M.O)

let test = 
  let circuit,sim,i,o,_ = N2.make "test_mem" M.f in
  let sim, waves = Waveterm_sim.wrap sim in
  HardCamlLlvmsim.Sim.write "" circuit;

  let open M.X.I in
  let open M.I in
  let open M.O in
  S.reset sim;

  S.cycle sim;
  for j=0 to 15 do
    i.r.w := B.consti X.abits j;
    i.r.d := B.consti X.dbits (15-j);
    i.r.we := B.vdd;
    S.cycle sim;
  done;
  i.r.we := B.gnd;

  for j=0 to 15 do
    i.r.w := B.consti X.abits j;
    i.r.d := B.consti X.dbits j;
    i.we2 := B.vdd;
    S.cycle sim;
  done;
  i.we2 := B.gnd;

  for j=0 to 15 do
    i.r.r := B.consti X.abits j;
    S.cycle sim;
  done;

  for i=0 to 3 do
    S.cycle sim;
  done;
  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

