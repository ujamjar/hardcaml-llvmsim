open HardCaml
module B = Bits_ext.Comb.IntbitsList
module L = HardCamlLlvmsim.Sim.Make(B)
module S = Cyclesim.Api

module Waveterm_waves = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
module Waveterm_ui = HardCamlWaveLTerm.Ui.Make(B)(Waveterm_waves)

(*let () = HardCamlLlvmsim.Sim.test_bb()*)

(* llvmsim test *)
let test = 
  let open HardCaml.Signal.Comb in
  let open HardCaml.Signal.Seq in
  let bits = 5 in
  let a = input "a" bits in
  let b = input "b" bits in
  let c = a +: b +: (consti bits 1) in
(*  let d = c -: a in
  let e = d &: b in
  let f = e |: d in
  let sel = select c 1 0 in
  let g = mux sel [ c;d;e;f ] in
  let h = concat [g;g] in
  let i = reg r_full enable h in
  let o = output "o" i in*)
  let circuit = Circuit.make "llvm_test" [ output "c" c; output "c_reg" (reg r_full enable c)] in
  L.write "" circuit;
  let sim = L.make circuit in
  let sim, waves = Waveterm_sim.wrap sim in
  (*let vcd = false in
  let sim = if vcd then V.wrap stdout sim else sim in*)
  
  let a = S.in_port sim "a" in
  let b = S.in_port sim "b" in
  let enable = S.in_port sim "enable" in
  let clear = S.in_port sim "clear" in
  (*let o = S.out_port sim "o" in*)

  a := B.consti bits 1;
  b := B.consti bits 2;

  S.reset sim;
  clear := B.gnd;
  enable := B.vdd;
    
  let cycle() = 
    a := B.(+:) !a (B.consti bits 1);
    (*b := B.(-:) !b (B.consti bits 1);*)
    S.cycle sim;
  in

  for i=0 to 10 do
    cycle ();
  done;

  Lwt_main.run (Waveterm_ui.run Waveterm_waves.({ cfg=default; waves }))

