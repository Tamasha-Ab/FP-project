module Main (main) where

import Expr
import Text.Printf (printf)

type Sample = (String, Env, Expr, Either String Double)

samples :: [Sample]
samples =
  [ ( "Operator precedence represented by the expression tree",
      [],
      Add (Lit 2) (Mul (Lit 3) (Lit 4)),
      Right 14
    ),
    ( "Variable lookup",
      [("x", 7), ("y", 5)],
      Add (Var "x") (Var "y"),
      Right 12
    ),
    ( "Let binding",
      [],
      Let "x" (Lit 10) (Mul (Var "x") (Lit 2.5)),
      Right 25
    ),
    ( "Nested arithmetic",
      [],
      Div (Sub (Lit 30) (Lit 10)) (Lit 4),
      Right 5
    ),
    ( "Division by zero",
      [],
      Div (Lit 10) (Sub (Lit 3) (Lit 3)),
      Left "Division by zero"
    ),
    ( "Undefined variable",
      [],
      Add (Var "missing") (Lit 1),
      Left "Undefined variable: missing"
    ),
    ( "A Let binding shadows an outer variable",
      [("x", 100)],
      Let "x" (Lit 8) (Add (Var "x") (Lit 2)),
      Right 10
    )
  ]

printSample :: Int -> Sample -> IO ()
printSample number (description, env, expression, expected) = do
  let actual = eval env expression
      status = if actual == expected then "PASS" else "FAIL"
  printf "%d. %s [%s]\n" number description status
  putStrLn ("   Environment: " ++ show env)
  putStrLn ("   Expression:  " ++ show expression)
  putStrLn ("   Expected:    " ++ show expected)
  putStrLn ("   Actual:      " ++ show actual)

main :: IO ()
main = do
  putStrLn "Arithmetic Expression Interpreter"
  putStrLn "================================="
  putStrLn ""
  putStrLn "Sample evaluations (expected versus actual):"
  mapM_ (uncurry printSample) (zip [1 ..] samples)

  let expressionToSimplify =
        Mul
          (Add (Var "x") (Lit 0))
          (Mul (Lit 1) (Add (Lit 2) (Lit 3)))
      simplifiedExpression = simplify expressionToSimplify

  putStrLn ""
  putStrLn "Simplification example:"
  putStrLn ("   Before: " ++ show expressionToSimplify)
  putStrLn ("   After:  " ++ show simplifiedExpression)
  putStrLn ("   Before evaluates to: " ++ show (eval [("x", 4)] expressionToSimplify))
  putStrLn ("   After evaluates to:  " ++ show (eval [("x", 4)] simplifiedExpression))

  let batchExpressions = map (\(_, _, expression, _) -> expression) samples
      allResults = evalAll [] batchExpressions
      successfulResults = evalBatch [] batchExpressions
      (successCount, failureCount) = batchSummary [] batchExpressions

  putStrLn ""
  putStrLn "Batch evaluation example (using an empty environment):"
  putStrLn ("   All results:       " ++ show allResults)
  putStrLn ("   Successful values: " ++ show successfulResults)
  printf "   Summary: %d succeeded, %d failed\n" successCount failureCount
