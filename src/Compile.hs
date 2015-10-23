{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Compile (Compile, runCompileMonad)  where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.Map (Map)

newtype Compile s a
   = Compile (StateT s IO a)
   deriving (Monad, MonadState s, MonadIO)

runCompileMonad :: Compile s a -> s -> IO a
runCompileMonad (Compile comp) = evalStateT comp

