module Hw3 where
import Data.List
type Symbol = String
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq 
 
instance Show Expr where
        show (Var x) = x
        show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
        show (Lambda x y) = "(\\" ++ x ++ "." ++ show y ++ ")"
 
changeName used = head $ filter (\x -> not ( x `elem` used)) ([['a'] ++ show i | i <- [0..]])
 
freeVars (Var x) = [x]
freeVars (App e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Lambda x e) = freeVars e \\ [x]
 
substitute x expr = sub where
  sub (Var y) | x == y = expr | otherwise = Var y
  sub (App e1 e2) = App (sub e1) (sub e2)
  sub l@(Lambda y e) | x == y = l
                     | otherwise = let y' = if y `elem` freeVars expr then changeName (freeVars expr `union` freeVars e) else y
                                    in Lambda y' (sub $ if y == y' then e else substitute y (Var y') e)
 
--  | otherwise = let used = freeVars expr `union` freeVars e
--                                          y' = if y `elem` freeVars expr then changeName used else y
--                                      in Lambda y' (sub $ substitute y (Var y') e)

evalHelper (Var x) = Var x
evalHelper (App (Lambda x y) z) = substitute x z y
evalHelper (App x y) = App x' y' where 
                          x' = evalHelper x
                          y' = evalHelper y
evalHelper (Lambda x y) = Lambda x (evalHelper y)

eval expr | reduced == expr = expr 
          | otherwise = eval reduced
          where reduced = evalHelper expr

yCombinator :: Expr
yCombinator =
  Lambda
    "f"
    ( App
        (Lambda "x" (App (Var "f") (App (Var "x") (Var "x"))))
        (Lambda "x" (App (Var "f") (App (Var "x") (Var "x"))))
    )

sumFirstFive :: Expr
sumFirstFive =
  App
    yCombinator
    ( Lambda
        "sum"
        ( Lambda
            "n"
            ( Lambda
                "acc"
                ( App
                    ( App
                        (App (Var "ifZero") (Var "n"))
                        (Var "acc")
                    )
                    ( App
                        (App (Var "sum") (App (Var "pred") (Var "n")))
                        (App (App (Var "plus") (Var "n")) (Var "acc"))
                    )
                )
            )
        )
    )

ifZero = Lambda "n" (Lambda "x" (Lambda "y" (App (App (Var "n") (Var "y")) (Var "x")))) -- Mockup

pred = Lambda "n" (Lambda "f" (Lambda "x" (App (App (Var "n") (Lambda "g" (Lambda "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Lambda "u" (Var "x"))))) -- Mockup

plus = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))) -- Mockup