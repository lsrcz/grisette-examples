# datalab

This example is inspired by the [CS:APP Data Lab](http://csapp.cs.cmu.edu/3e/labs.html). In this example, we build a simple verifier and synthesizer for bit-level programs.

The verifier is similar to the `btest` program shipped with the datalab handout, and it checks whether a given program satisfies the specification (description of the expected behavior).

The synthesizer automatically generates a program that satisfies a given specification. It is not a strong synthesizer, so do not expect that it can easily help you with all your homework assignments :).

## How to use

Simply type the following command in the terminal.

```bash
stack run datalab
```

You should have a working installation of [Stack](https://docs.haskellstack.org/en/stable/README/), and a [boolector](https://github.com/Boolector/boolector) solver available in PATH.

You may change the code to use z3 solver, too, but you should expect that the synthesizer will be much slower as boolector is a much better solver for bitvectors.

## What is in this example

The example contains the following files:

- `src/Program.hs`: The definition of a small DSL for bit-level programs, and a symbolic interpreter that translates it into SMT formulas.
- `src/Builder.hs`: Some helper type classes for building bit-level programs.
- `src/Verifier.hs`: The verifier that checks whether a given program satisfies a given specification.
- `src/Synthesizer.hs`: The synthesizer that automatically generates a program that satisfies a given specification.
- `app/Main.hs`: We have the specification and a implementation for `bitXor` and `isLessOrEqual` programs and use the verifier to verify that they are correct on all integers. We also have a program sketch for `bitXor` program, which specifies a space of candidate programs, and we use the synthesizer to fill in the missing parts of the program sketch.
