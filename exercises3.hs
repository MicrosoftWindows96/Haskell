{-# LANGUAGE DeriveGeneric #-}

module Exercises (Expr(..),show,toCNF,resolve,valid) where

import Data.List (nub, partition,find, delete)
import Data.Maybe (mapMaybe)


data Expr = Var Char | 
            Not Expr | 
            And [Expr] | 
            Or [Expr] deriving (Eq, Ord)

-- Imports --
instance Show Expr where
    show (Var c) = [c]
    show (Not e) = "~" ++ case e of
                        And _ -> "(" ++ show e ++ ")"
                        Or _  -> "(" ++ show e ++ ")"
                        _     -> show e
    show (And es) = case es of
                      []  -> "T"
                      [e] -> show e
                      _   -> showListExpr " ^ " es
    show (Or es) = case es of
                      []  -> "F"
                      [e] -> show e
                      _   -> showListExpr " v " es

showListExpr :: String -> [Expr] -> String
showListExpr _ []     = ""
showListExpr _ [e]    = show e
showListExpr sep list = foldr1 (\a b -> a ++ sep ++ b) (map showNestedExpr list)

showNestedExpr :: Expr -> String
showNestedExpr e@(And []) = show e
showNestedExpr e@(Or [])  = show e
showNestedExpr e@(And [_]) = show e
showNestedExpr e@(Or [_])  = show e
showNestedExpr e@(And _) = "(" ++ show e ++ ")"
showNestedExpr e@(Or _)  = "(" ++ show e ++ ")"
showNestedExpr e         = show e

-- Exercise A8
toNNF :: Expr -> Expr
toNNF (Var x) = Var x
toNNF (Not (Var x)) = Not (Var x)
toNNF (Not (Not e)) = toNNF e
toNNF (Not (And es)) = Or (map (toNNF . Not) es)
toNNF (Not (Or es)) = And (map (toNNF . Not) es)
toNNF (And es) = And (map toNNF es)
toNNF (Or es) = Or (map toNNF es)

distribute :: Expr -> Expr
distribute (Or [e]) = distribute e
distribute (Or (e:es)) = distributeOr e (distribute (Or es))
distribute (And es) = And (map distribute es)
distribute e = e

distributeOr :: Expr -> Expr -> Expr
distributeOr (And es1) (And es2) = And [distribute (Or [e1, e2]) | e1 <- es1, e2 <- es2]
distributeOr (And es) e = And (map (\x -> distribute (Or [x, e])) es)
distributeOr e (And es) = And (map (\x -> distribute (Or [e, x])) es)
distributeOr e1 e2 = Or [e1, e2]

wrapSimple :: Expr -> Expr
wrapSimple e@(Var _) = And [Or [e]]
wrapSimple e@(Not (Var _)) = And [Or [e]]
wrapSimple (Or es) = if all isLiteral es then And [Or es] else Or es
wrapSimple (And es) = if all isLiteral es then And (map (\x -> Or [x]) es) else And es
wrapSimple e = e

isLiteral :: Expr -> Bool
isLiteral (Var _) = True
isLiteral (Not (Var _)) = True
isLiteral _ = False

toCNF :: Expr -> Expr
toCNF e@(Var _) = And [Or [e]]
toCNF e@(Not (Var _)) = And [Or [e]]
toCNF e@(Or es) 
    | all isLiteral es = And [Or es]
    | otherwise = wrapSimple . distribute . toNNF $ e
toCNF e = wrapSimple . distribute . toNNF $ e

-- Exercise A9
resolve :: Expr -> Expr -> Expr
resolve (Or es1) (Or es2) =
    let
        combined = es1 ++ es2
        filtered = filter (not . flip elem combined . negateExpr) combined
    in
        if length filtered < length combined
            then Or (nub filtered)
            else error "No complementary literals found"
    where
        negateExpr (Not x) = x
        negateExpr x = Not x
resolve _ _ = error "Arguments must be disjunctions"

valid :: [Expr] -> Expr -> Bool
valid es e =
    let
        negatedE = toCNF $ Not e
        cnfExprs = map toCNF es ++ [negatedE]
        clauses = nub $ concatMap (\(And cs) -> cs) cnfExprs
    in
        resolveClauses clauses

resolveClauses :: [Expr] -> Bool
resolveClauses clauses
    | Or [] `elem` clauses = True
    | otherwise = 
        let resolvablePairs = [(c1, c2) | c1 <- clauses, c2 <- clauses, c1 /= c2, isResolvable c1 c2]
            resolvedClauses = nub [resolve c1 c2 | (c1, c2) <- resolvablePairs]
            newClauses = nub (clauses ++ resolvedClauses)
        in if length newClauses == length clauses
           then False
           else resolveClauses newClauses

isResolvable :: Expr -> Expr -> Bool
isResolvable (Or es1) (Or es2) = any (\e -> negateExpr e `elem` es2) es1
    where
        negateExpr (Not x) = x
        negateExpr x = Not x
isResolvable _ _ = False