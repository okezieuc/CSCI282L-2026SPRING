# Diamondback Compiler

## Calling Convention

Function calls in Diamondback use the form `(<name> <expr>*)`.

For example, `(f a b c)` calls the function `f` with three arguments. The expressions `a`, `b`, and `c` are evaluated, those values are passed to `f`, and the call expression produces the value returned by `f`.

Function definitions use the form `(fun (<name> <param>*) <expr>)`. The parameter names in the definition are bound to the argument values from the call, and the body expression computes the result of the function.

## Example Programs

Sample programs are provided in the `/test` directory that calculate factorial, fibonacci numbers, and check whether numbers are even or odd.
