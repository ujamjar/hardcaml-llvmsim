open HardCaml

module Seq = Signal.Make_seq(struct
  let reg_spec = Signal.Seq.r_sync
  let ram_spec = Signal.Seq.r_none
end)

module Fifo = struct
  open Signal.Comb
  let bits = 4
  module I = interface clear[1] wr[1] d[bits] rd[1] end
  module O = interface q[bits] end
  let f i =
    let open I in
    let size = 1 lsl bits in
    let wa = Seq.reg_fb ~c:i.clear ~e:i.wr ~w:bits (fun d -> d +:. 1) -- "fifo_wa" in
    let ra = Seq.reg_fb ~c:i.clear ~e:i.rd ~w:bits (fun d -> d +:. 1) -- "fifo_ra" in
    let q = Seq.ram_rbw size ~we:i.wr ~wa ~d:i.d ~re:i.rd ~ra in
    O.{ q }
end

module B = Bits_ext.Comb.IntbitsList
module S = Cyclesim.Api
module Cl = HardCamlLlvmsim.Sim.Gen(B)(Fifo.I)(Fifo.O)
module Cs = HardCaml.Interface.Gen(B)(Fifo.I)(Fifo.O)
module Cs' = HardCaml.Cyclesim.Make(B)

module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

let test = 
  let circuit,sim,i,o = Cl.make "test_fifo_llvm" Fifo.f in
  HardCamlLlvmsim.Sim.write "" circuit;
  let _,sim',_,_ = Cs.make "test_fifo_cs" Fifo.f in
  let sim = Cs'.combine_strict sim sim' in
  let sim, waves = Waveterm_sim.wrap sim in

  let open Fifo.I in
  let open Fifo.O in

  S.reset sim;
  
  let cycle () = try S.cycle sim with _ -> () in

  for j=0 to 3 do
    i.d := B.consti Fifo.bits j;
    i.wr := B.vdd;
    cycle ();
  done;
  i.d := B.consti Fifo.bits 0;
  i.wr := B.gnd;
  for j=0 to 3 do
    i.rd := B.vdd;
    cycle ();
  done;
  i.rd := B.gnd;

  cycle ();
  cycle ();

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

