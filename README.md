# Overview

[![Build Status](https://travis-ci.org/ujamjar/hardcaml-llvmsim.svg?branch=master)](https://travis-ci.org/ujamjar/hardcaml-llvmsim)

Compiles a circuit to a high speed, native code, cycle accurate simulation model
using LLVM.

The simulation models may also be stored to a bitcode file and reloaded.

# Usage

The built in ocaml simulator is constructed with;

```
module Bits = HardCaml.Bits.Comb.IntbitsList
module Sim = HardCaml.Cyclesim.Make(Bits)
let sim = Sim.make circuit
```

The LLVM based simulator is constructed with;

```
module Bits = HardCaml.Bits.Ext.Comb.IntbitsList
module Sim = HardCamlLlvmsim.Sim.Make(Bits)
let sim = Sim.make circuit
```

The resulting types of `sim` are the same so testbenchs remain the same.

The interface module (Bits) must come from `HardCaml.Bits.Ext` - these are a 
strict extension of `HardCaml.Bits` and are compatible so we can still `combine`
simulators

```
module Bits = HardCaml.Bits.Ext.Comb.IntbitsList
module Sim1 = HardCaml.Cyclesim.Make(Bits)
module Sim2 = HardCamlLlvmsim.Sim.Make(Bits)
let sim1 = Sim1.make circuit
let sim2 = Sim2.make circuit
let sim = Sim1.combine sim1 sim2
```

This will run both simulators simultaneously and check that they match exactly.

An `Interface` based flow is also provided

```
module Design = struct
  module I = interface ... end
  module O = interface ... end
  let f i = ... logic ...
end

module D = HardcamlLlvmsim.Sim.Gen(Bits)(Design.I)(Design.O)
let circuit, sim, inports, outports = D.make "the_design" Design.f
```

