{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter1 where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Expr_Parser as P (parseExpr, Language (..))

data Expr = Lit Integer
          | Expr :+: Expr
          | Var Name            -- new
          | Let Name Expr Expr  -- new
  deriving (Show)

type Name = String
type Value = Integer

-- | An environment maps variables to values
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty