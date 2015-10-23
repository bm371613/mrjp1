module Compile (Compile, runCompileMonad)  where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..), StateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)

newtype Compile s a
   = Compile (StateT s IO a)

instance Monad (Compile s) where
    return = Compile . return
    (Compile sm) >>= k = Compile $ sm >>= (\x -> case k x of Compile sk -> sk)
    fail = Compile . fail

-- TODO instance MonadState s (Compile s) where

instance MonadIO (Compile s) where
    liftIO = Compile . liftIO

runCompileMonad :: Compile s a -> s -> IO a
runCompileMonad (Compile comp) = evalStateT comp

