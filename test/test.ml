open HardCaml

module Seq = Signal.Make_seq(struct
  let reg_spec = Signal.Seq.r_sync
  let ram_spec = Signal.Seq.r_none
end)

module Test = struct
  open Signal.Comb
  module I = interface a[8] b[8] sel[1] end
  module O = interface
    add[8] sub[8] mux[8] select[4] concat[16]
    mul[16] lt[1] and_[8] or_[8] xor_[8] not_[8]
    reg[8] pipe[8] counter[4]
  end
  let f i = 
    let open I in
    O.{
      add = i.a +: i.b;
      sub = i.a -: i.b;
      mux = mux2 i.sel i.a i.b;
      select = select i.a 5 2;
      concat = i.a @: i.b;
      mul = i.a *: i.b;
      lt = i.a <: i.b;
      and_ = i.a &: i.b;
      or_ = i.a |: i.b;
      xor_ = i.a ^: i.b;
      not_ = ~: (i.a);
      reg = Seq.reg ~e:vdd i.a;
      pipe = Seq.pipeline ~n:2 ~e:vdd i.a;
      counter = Seq.reg_fb ~e:i.sel ~w:4 (fun d -> d +:. 1);
    }
end

module B = Bits_ext.Comb.IntbitsList
module S = Cyclesim.Api

module Cl = HardCamlLlvmsim.Sim.Gen(B)(Test.I)(Test.O)
module Cs = HardCaml.Interface.Gen(B)(Test.I)(Test.O)
module Cs' = HardCaml.Cyclesim.Make(B)

module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

let sim_llvm () = Cl.make "test_llvm" Test.f
let sim_cs () = Cs.make "test_cs" Test.f
let sim_combine () = 
  let c,s,i,o = Cl.make "test_llvm" Test.f in
  let c,s',_,_ = Cs.make "test_cs" Test.f in
  c, Cs'.combine_strict s s', i, o

(* llvmsim test *)
let test = 
  let _,sim,i,o = 
    match `cs with
    | `cs -> sim_cs () 
    | `llvm -> sim_llvm ()
    | `combine -> sim_combine ()
  in
  let sim, waves = Waveterm_sim.wrap sim in

  let open Test.I in
  let open Test.O in

  S.reset sim;
  
  for j=0 to 7 do
    i.a := B.consti 8 ((j+1)*2);
    i.b := B.consti 8 (j+1);
    i.sel := B.consti 1 (j mod 2);
    S.cycle sim
  done;
  S.cycle sim;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

