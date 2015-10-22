{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Monad (Compile (..), runCompileMonad)  where

import Control.Monad.State.Strict as State hiding (State)
import Control.Applicative (Applicative (..))

import Types (CompileState (..))

newtype Compile a
   = Compile (StateT CompileState IO a)
   deriving (Monad, Functor, MonadIO, Applicative)

instance MonadState CompileState Compile where
   get = Compile get
   put s = Compile $ put s

runCompileMonad :: Compile a -> CompileState -> IO a
runCompileMonad (Compile comp) = evalStateT comp:
