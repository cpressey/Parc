module Unification where

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

fetch name Failed = Nothing
fetch name (Env []) = Nothing
fetch name (Env ((cand, t):rest)) = if name == cand then Just t else fetch name (Env rest)

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
        else case fetch name env of
            Just boundTo ->
                unify boundTo pattern env
            Nothing ->
                update name pattern env
