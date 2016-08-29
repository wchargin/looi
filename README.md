# looi [![Build Status](https://travis-ci.org/wchargin/looi.svg?branch=master)](https://travis-ci.org/wchargin/looi)

A toy Scheme-like language.

## Building

```
$ stack build
$ stack test
$ stack exec looi <<< "((func x y (+ (* x x) (* y y))) 3 4)"  # ==> 25
```

## Specification

The concrete syntax of the language ascribes to the following EBNF:

```
  LOOI4         =               num
                |               "true"
                |               "false"
                |               id
                |               { "new-array" LOOI4 LOOI4 }
                |               { "ref" LOOI4 "[" LOOI4 "]" }
                |               { LOOI4 "[" LOOI4 "]" "<-" LOOI4 }
                |               { id "<-" LOOI4 }
                |               { "begin" LOOI4 LOOI4 ... }
                |               { "if" LOOI4 LOOI4 LOOI4 }
                |               { "with" { id "=" LOOI4 } ... LOOI4 }
                |               { "func" id ... LOOI4 }
                |               { operator LOOI4 LOOI4 }
                |               { LOOI4 LOOI4 ... }

  operator      =               "+"
                |               "-"
                |               "*"
                |               "/"
                |               "eq?"
                |               "<="
```

…where an `id` is any symbol not `true`, `false`, `with`, `if`, `func`,
`new-array`, `=`, `<-`, `begin`, or one of the operator names.

It is acceptable to substitute any matching delimiters of `()` `[]` `{}` for
the braces around expressions.

## Semantics

### Forms

  - `n`, where `n` is a decimal literal, is a literal number.

  - `true` and `false` are literal boolean values.

  - `x`, where `x` is an identifier, is a variable reference that evaluates to
    the value bound to `x`; if `x` is unbound, this yields an error.

  - `{new-array n v}` creates and evaluates to a new array of length `v`, all
    initialized to `v`.

  - `{ref a [n]}` evaluates to the `n`th element of `a`, zero-based;
    out-of-bounds indices yield an error.

  - `{a [n] <- v}` sets the `n`th element of `a`, zero-based, to `v`;
    out-of-bounds indices yield an error.

  - `{x <- v}` changes the binding of `x` to `v`, and evaluates to `v`; if
    if `x` is not already bound, this yields an error.

  - `{begin e1 ... en}` evaluates each `ei` in the provided order, and
    evaluates to the value to which `en` evaluates.

  - `{if g t e}` evaluates to the result of evaluating `t` if `g` evaluates
    to `true`, or the result of evaluating `e` if `g` evaluates to `false`;
    otherwise, if `g` evaluates to a non-boolean value, this raises an error.

  - `{with {x1 = v1} ... {xn = vn} e}` evaluates to the result of
    evaluating `e` in a context where each `xi` is bound to the result of
    evaluating the corresponding `vi`

  - `{func x1 ... xn e}` is a lambda expression whose parameters are the `xi`
    (of which there may be none) and whose body is `e`.

  - `{op a b}`, where `op` is an operator, performs the evident operation.
  

### Notes

  - Evaluation is strict.
  - Arrays are untyped.
  - It is permitted to use `eq?` to compare values of different types; the
    result will be `false`.
  - When either argument to `eq?` is a closure, the result is `false`.
