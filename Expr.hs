module Expr
  ( Expr (..),
    Env,
    eval,
    simplify,
    evalAll,
    evalBatch,
    batchSummary,
  )
where

-- | Arithmetic expressions supported by the interpreter.
--
-- 'Let' is the extension constructor required by Part A of the assignment.
-- @Let name value body@ means: let @name = value@ while evaluating @body@.
data Expr
  = Lit Double
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Let String Expr Expr
  deriving (Eq, Show)

-- | Variable bindings. A binding nearer the front shadows later bindings.
type Env = [(String, Double)]

-- | Evaluate an expression in an environment.
--
-- Errors are returned as 'Left' values, so evaluation never needs to throw an
-- exception for an undefined variable or division by zero.
eval :: Env -> Expr -> Either String Double
eval _ (Lit value) = Right value
eval env (Var name) =
  case lookup name env of
    Just value -> Right value
    Nothing -> Left ("Undefined variable: " ++ name)
eval env (Add left right) = do
  leftValue <- eval env left
  rightValue <- eval env right
  Right (leftValue + rightValue)
eval env (Sub left right) = do
  leftValue <- eval env left
  rightValue <- eval env right
  Right (leftValue - rightValue)
eval env (Mul left right) = do
  leftValue <- eval env left
  rightValue <- eval env right
  Right (leftValue * rightValue)
eval env (Div numerator denominator) = do
  numeratorValue <- eval env numerator
  denominatorValue <- eval env denominator
  if denominatorValue == 0
    then Left "Division by zero"
    else Right (numeratorValue / denominatorValue)
eval env (Let name valueExpression body) = do
  value <- eval env valueExpression
  eval ((name, value) : env) body

-- | Recursively simplify an expression using algebraic identities and safe
-- constant folding. The rules do not discard a subexpression that might
-- produce an evaluation error.
simplify :: Expr -> Expr
simplify (Lit value) = Lit value
simplify (Var name) = Var name
simplify (Add left right) =
  case (simplify left, simplify right) of
    (expression, Lit 0) -> expression
    (Lit 0, expression) -> expression
    (Lit x, Lit y) -> Lit (x + y)
    (simpleLeft, simpleRight) -> Add simpleLeft simpleRight
simplify (Sub left right) =
  case (simplify left, simplify right) of
    (expression, Lit 0) -> expression
    (Lit x, Lit y) -> Lit (x - y)
    (simpleLeft, simpleRight) -> Sub simpleLeft simpleRight
simplify (Mul left right) =
  case (simplify left, simplify right) of
    (expression, Lit 1) -> expression
    (Lit 1, expression) -> expression
    (Lit x, Lit y) -> Lit (x * y)
    (simpleLeft, simpleRight) -> Mul simpleLeft simpleRight
simplify (Div numerator denominator) =
  case (simplify numerator, simplify denominator) of
    (expression, Lit 1) -> expression
    (Lit x, Lit y)
      | y /= 0 -> Lit (x / y)
    (simpleNumerator, simpleDenominator) ->
      Div simpleNumerator simpleDenominator
simplify (Let name valueExpression body) =
  Let name (simplify valueExpression) (simplify body)

-- | Evaluate every expression and retain both successes and errors.
--
-- @eval env@ is a partial application: it produces the function passed to
-- 'map'. This demonstrates currying and higher-order function use.
evalAll :: Env -> [Expr] -> [Either String Double]
evalAll env = map (eval env)

-- | Evaluate a batch and keep only the successful values.
--
-- This uses 'foldr' over the results produced by 'evalAll'.
evalBatch :: Env -> [Expr] -> [Double]
evalBatch env = foldr keepSuccess [] . evalAll env
  where
    keepSuccess (Right value) values = value : values
    keepSuccess (Left _) values = values

-- | Count successful and failed evaluations as @(successes, failures)@.
batchSummary :: Env -> [Expr] -> (Int, Int)
batchSummary env = foldr countResult (0, 0) . evalAll env
  where
    countResult (Right _) (successes, failures) = (successes + 1, failures)
    countResult (Left _) (successes, failures) = (successes, failures + 1)
