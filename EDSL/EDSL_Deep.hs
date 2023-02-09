{-# LANGUAGE GADTs #-}
{-|
  A simple embedded language for input/output. Deep embedding.
-}
module EDSL_Deep1 where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

type Input  = String 
type Output = String

--Types
data Program a where
  Return :: a -> Program a
  Put   :: Char -> Program ()
  Get   :: Program (Maybe Char)
  Bind   :: Program a -> (a -> Program b) -> Program b

type IOSem a = Input -> (a, Input, Output) 
-- | Run function
run :: Program a -> IOSem a
run (Put c)   input  = ((), input, c:"")
run Get        ""     = (Nothing, "", "")
run Get        (c:cs) = (Just c, cs, "")
run (Return x) input  = (x, input, "")
run (Bind p g) input  = (someb, someinp', someoutp ++ someoutp')
    where (somea, someinp, someoutp) = run p input
          pb = g somea
          (someb, someinp', someoutp') = run pb someinp

putC = Put
getC = Get

echo :: Program ()
echo = getC >>= f
    where f Nothing = return ()
          f (Just c) = putC c

instance Monad Program where
  return  =  Return
  (>>=)   =  Bind

instance Functor Program where
  fmap = liftM

instance Applicative Program where
  pure = return