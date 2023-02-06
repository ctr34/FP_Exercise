module Interpreter0 where 

import Control.Applicative
import Control.Monad.Identity as CMI

data Expr =   Lit Integer
            | Expr :+: Expr
    deriving Show

type Eval a = Identity a

-- | evaluation without monadic operation
sEval :: Expr -> Integer
sEval (Lit n)     = n
sEval (e1 :+: e2) = sEval e1 + sEval e2 

-- | evaluation with monad identity
sEval' :: Expr -> Eval Integer
sEval' (Lit n)     = return n
sEval' (e1 :+: e2) = (+) <$> sEval' e1 <*> sEval' e2

runEval :: Eval a -> a
runEval = CMI.runIdentity

e = Lit 8 :+: Lit 9