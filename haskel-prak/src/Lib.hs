{-# LANGUAGE BlockArguments #-}
module Lib where

import Data.Bifunctor
import Data.Char

-- Types
data Token = LVar String | LOpen | LClose | LLambda | LDot | LErr | LSpace | LEnd deriving (Eq)

newtype Variable = Variable String

data Expr = Var Variable | Lambda Variable Expr | App Expr Expr | Err [Token] | Empty



-- Parser
lSplit :: String -> (String, String)
lSplit [] = ([], [])
lSplit (x : xs) = if isAlpha x || isDigit x then first (x :) (lSplit xs) else ([], x : xs)

tokenize :: Char -> Token
tokenize '(' = LOpen
tokenize ')' = LClose
tokenize '\\' = LLambda
tokenize '.' = LDot
tokenize ' ' = LSpace
tokenize _ = LErr

lStep :: String -> [Token]
lStep [] = [LEnd]
lStep (x : xs)
  | tokenize x == LErr = if isAlpha x then LVar (fst (lSplit (x : xs))) : lStep (snd (lSplit (x : xs))) else LErr : lStep xs
  | tokenize x == LSpace = lStep xs
  | otherwise = tokenize x : lStep xs

split :: Char -> [Token] -> Int -> [Token]
split _ [] _ = []
split flag (op : xs) i
  | op == LClose && i == 0 = if flag == 'L' then [] else xs
  | op == LClose || op == LOpen = if flag == 'L' then op : split flag xs (i + if op == LClose then -1 else 1) else split flag xs (i + if op == LClose then -1 else 1)
  | otherwise = if flag == 'L' then op : split flag xs i else split flag xs i

appSplit :: [Token] -> ([Token], [Token])
appSplit [] = ([LEnd], [])
appSplit (LOpen : xs) = ([LOpen] ++ split 'L' xs 0 ++ [LClose], split 'R' xs 0)
appSplit (LLambda : (LVar x) : LDot : xs) = first ([LLambda, LVar x, LDot] ++) (appSplit xs)
appSplit ((LVar x) : xs) = ([LVar x], xs)
appSplit _ = ([LErr], [LErr])

parse :: [Token] -> Expr
parse [] = Empty
parse [LErr] = Err [LErr]
parse (LOpen : xs) = if leftRes == [LErr] || rightRes == [LErr] then Err [LErr] else App (parse leftRes) (parse rightRes)
  where
    leftRes = fst (appSplit (split 'L' xs 0))
    rightRes = snd (appSplit (split 'L' xs 0))
parse (LLambda : (LVar x) : LDot : xs) = Lambda (Variable x) (parse xs)
parse ((LVar x) : xs) = Var (Variable x)
parse xs = Err xs

-- Execution
betaReduction :: Expr -> Variable -> Expr -> Expr
betaReduction (Var (Variable x)) (Variable y) e = if x == y then e else Var (Variable x)
betaReduction (Lambda (Variable x) ek) y e = Lambda (Variable x) (betaReduction ek y e)
betaReduction (App e1 e2) y e = App (betaReduction e1 y e) (betaReduction e2 y e)
betaReduction _ _ _ = Err [LErr]

exprLazy :: Expr -> Expr
exprLazy (Err x) = Err x
exprLazy (Var (Variable x)) = Var (Variable x)
exprLazy (Lambda (Variable x) e) = Lambda (Variable x) e
exprLazy (App (Lambda (Variable x) e) e1) = betaReduction e (Variable x) e1
exprLazy (App e1 e2) = App (exprLazy e1) (exprLazy e2)
exprLazy _ = Err [LErr]

exprStr :: Expr -> String
exprStr (Var (Variable x)) = x ++ " "
exprStr (Lambda (Variable x) e) = "λ" ++ x ++ "." ++ exprStr e
exprStr (App e1 e2) = "(" ++ exprStr e1 ++ exprStr e2 ++ ")"
exprStr _ = "Input error."

lazy :: Expr -> String
lazy (Err x) = "Input error"
lazy x
  | exprStr x /= exprStr (exprLazy x) = lazy (exprLazy x)
  | otherwise = exprStr x

alphaConversion :: Expr -> Int -> (Expr, Int)
alphaConversion (Var (Variable x)) i = (Var (Variable x), i)
alphaConversion (Lambda (Variable x) e) i = first (Lambda (Variable ("v" ++ show (i + 1)))) (alphaConversion (betaReduction e (Variable x) (Var (Variable ("v" ++ show (i + 1))))) (i + 1))
alphaConversion (App e1 e2) i = (App expr1 expr2, k)
  where
    (expr1, j) = alphaConversion e1 i
    (expr2, k) = alphaConversion e2 j
alphaConversion _ _ = (Err [LErr], -1)

reduce :: String -> String
reduce str = lazy (fst (alphaConversion (parse if LErr `elem` list then [LErr] else list) 0))
  where
    list = lStep str
