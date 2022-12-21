module ParcUnif where
import Prelude hiding (pred, seq, any)

import ParcSt
import Unification

--
-- Highly Experimental
--

-- TODO: this would be more readable as a record
data Unif = Unif Unifier String

freshVar (Unif u n) = n ++ "a"  -- FIXME: get fresh var from looking at u

fresh :: (String -> Parser Unif) -> Parser Unif
fresh f =
    (\st -> case st of
        Failure -> Failure
        Parsing s (Unif u n) ->
            let
                n'  = freshVar (Unif u n)
                st' = Parsing s (Unif u n')
                p   = f n'
            in
                p st')

--
-- Demo
--

char x = pred (\c v -> if x == c then Just v else Nothing)
try c = alt c ok
many1 c = seq c (many c)

c = char 'c'
d = char 'd'

-- Demo
test1 = seq c (seq c d)
