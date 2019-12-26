# Monadic Interpreter
A simple interpreter with typechecker for F♭, a pure functional language,
implemented using monads.

### How to Build

run `build.sh`

### How to Clean

run `clean.sh`

## Using the interpreter

run `./Fb`, then simply enter the expression you wish to evaluate.

Example:

```ocaml
(Function x -> x) (Function y -> y) 1
```

## Notes
The full F♭ language specification is available at Principles of Programming Languages
by Dr. Scott F. Smith, in section 2.3
