module Main

import Effects
import Effect.State
import Effect.Exception
import Effect.Random
import Effect.StdIO

data Expr = Var String
          | Val Integer
          | Add Expr Expr
          | Random Integer 

Env : Type
Env = List (String, Integer)

ExprProg : Type -> Type
ExprProg t = Eff t [EXCEPTION String, STDIO, RND, STATE Env]

getRnd : Integer -> Eff Integer [STDIO, RND]
getRnd upper = do val <- rndInt 0 upper
                  print val
                  return val

eval : Expr -> ExprProg Integer
eval (Var x) 
   = case lookup x !get of
          Nothing => raise ("No such variable " ++ x)
          Just val => pure val
eval (Val x) = pure x
eval (Add l r) = [| eval l + eval r |]
eval (Random x) = getRnd x

testExpr : Expr
testExpr = Add (Add (Var "foo") (Val 42)) (Random 100)

runEval : List (String, Integer) -> Expr -> IO Integer
runEval args expr = run (eval' expr)
  where eval' : Expr -> ExprProg Integer
        eval' e = do put args
                     srand 1234567890
                     eval e

main : IO ()
main = do putStr "Number: "
          x <- getLine
          val <- runEval [("foo", cast x)] testExpr
          putStrLn $ "Answer: " ++ show val

