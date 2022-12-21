module Unification where

import Prelude hiding (lookup)

--
-- Terms...
--

data Term = Term String [Term]
          | Var String
    deriving (Show, Ord, Eq)

--
-- Unifiers...
--

data Unifier = Env [(String, Term)]
             | Failed
    deriving (Show, Ord, Eq)

empty = Env []

lookup name Failed = Nothing
lookup name (Env []) = Nothing
lookup name (Env ((cand, t):rest)) = if name == cand then Just t else lookup name (Env rest)

update name value (Env rest) = Env ((name, value):rest)
update name value Failed = Failed

--
-- *Unification*
--

unify s t Failed = Failed
unify s@(Var v) t env = matchVar s t env
unify s t@(Var v) env = matchVar t s env
unify s@(Term sn ss) t@(Term tn ts) env = if sn == tn then unifyAll ss ts env else Failed
    where
        unifyAll [] [] env = env
        unifyAll (s:ss) (t:ts) env = let env' = unify s t env in unifyAll ss ts env'
        unifyAll _ _ _ = Failed

matchVar var@(Var name) pattern env =
    if var == pattern then env
        else case lookup name env of
            Just boundTo ->
                unify boundTo pattern env
            Nothing ->
                update name pattern env

--
-- Demo
--

test1 =
    let
        t1 = Term "x" [Var "y", Term "z" []]
        t2 = Term "x" [Term "n" [Var "b"], Var "x"]
    in
        unify t1 t2 empty
        -- == Env [("x",Term "z" []),("y",Term "n" [Var "b"])]
