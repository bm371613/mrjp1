{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Check (check, CheckState, emptyCheckState, runCheckMonad) where

import Control.Monad (unless)
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Data.Set (Set, empty, insert, member)

import Bnfc.Absgrammar

data CheckState = CheckState { checkStateVars :: Set String }

emptyCheckState = CheckState empty

newtype Check a
    = Check (StateT CheckState (Except String) a)
   deriving (Monad, MonadState CheckState, MonadError String)

runCheckMonad :: Check a -> CheckState -> Either String a
runCheckMonad (Check check) state = runExcept $ evalStateT check state

class Checkable a where
    check :: a -> Check ()

instance Checkable Program where
    check (Prog stmts) = mapM_ check stmts

instance Checkable Stmt where
    check (SAss (Ident s) _) = do
        state <- get
        put $ CheckState $ insert s $ checkStateVars state
        return ()
    check (SExp e) = check e

checkTwo :: (Checkable c1, Checkable c2) => c1 -> c2 -> Check ()
checkTwo c1 c2 = do
    check c1
    check c2
    return ()

instance Checkable Exp where
    check (ExpAdd e1 e2) = checkTwo e1 e2
    check (ExpSub e1 e2) = checkTwo e1 e2
    check (ExpMul e1 e2) = checkTwo e1 e2
    check (ExpDiv e1 e2) = checkTwo e1 e2
    check (ExpLit _) = return ()
    check (ExpVar (Ident s)) = do
        state <- get
        unless (member s $ checkStateVars state)
            $ throwError $ "Undefined variable: " ++ s

