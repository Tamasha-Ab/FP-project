# EC8206 Functional Programming Project

This project is a small, type-safe arithmetic expression interpreter written
in Haskell. It demonstrates algebraic data types, recursion, pattern matching,
explicit error handling, higher-order functions, currying, and immutability.

## Files

- `Expr.hs` defines the expression language and its interpreter.
- `Main.hs` runs seven expected-versus-actual sample evaluations, a
  simplification example, and a batch-evaluation example.

## Expression language

`Expr` supports:

- numeric literals and variable references;
- addition, subtraction, multiplication, and division; and
- `Let` bindings as the required extension constructor.

For example:

```haskell
Let "x" (Lit 10) (Add (Var "x") (Lit 5))
```

represents `let x = 10 in x + 5` and evaluates to `Right 15.0`.

The evaluator has the following type:

```haskell
eval :: Env -> Expr -> Either String Double
```

An undefined variable or division by zero produces a `Left` error value rather
than terminating the program with a runtime exception.

## Build and run

Run directly with `runghc`:

```text
runghc Main.hs
```

Alternatively, compile an executable with GHC:

```text
ghc -Wall Main.hs -o fp-project
./fp-project
```

On Windows PowerShell, run the compiled program with:

```text
.\fp-project.exe
```

No third-party Haskell packages are required.

## Assignment requirement coverage

- **Part A:** `Expr`, including `Let`, and the `Env` type are in `Expr.hs`.
- **Part B:** `eval` recursively pattern matches on every constructor and
  returns errors through `Either String Double`.
- **Part C:** `simplify` implements recursive algebraic rewriting and constant
  folding. `evalAll`, `evalBatch`, and `batchSummary` demonstrate `map`,
  `foldr`, currying, and partial application.
- **Testing:** `Main.hs` contains seven sample evaluations, including successful
  calculations, variable lookup, `Let`, shadowing, division by zero, and an
  undefined variable.

The separate written report must contain Parts D and E, a short version of
these run instructions, and at least five expected-versus-actual evaluations.
Any external or AI-assisted material should be acknowledged as required by the
assignment's academic-integrity instructions.
