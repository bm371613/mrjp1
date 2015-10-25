module Common (Compile, compilerMain) where

import Control.Monad (unless)
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState, StateT, evalStateT, get, state)
import Control.Monad.Trans.Class (lift)
import Data.Set (Set, empty, insert, member)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Bnfc.Absgrammar
import Bnfc.Lexgrammar
import Bnfc.Pargrammar
import Bnfc.ErrM

-- check

type CheckState = Set String
emptyCheckState = empty

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
    check (SAss (Ident ident) _) = state $ \s -> ((), insert ident s)
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
    check (ExpVar (Ident ident)) = do
        state <- get
        unless (member ident state)
            $ throwError $ "Undefined variable: " ++ ident

-- compile

newtype Compile s a = Compile (StateT s IO a)

instance Monad (Compile s) where
    return = Compile . return
    (Compile sm) >>= k = Compile $ sm >>= (\x -> case k x of Compile sk -> sk)
    fail = Compile . fail

instance MonadState s (Compile s) where
    state f = Compile $ state f

instance MonadIO (Compile s) where
    liftIO = Compile . liftIO

runCompileMonad :: Compile s a -> s -> IO a
runCompileMonad (Compile sm) = evalStateT sm

-- main

compilerMain :: (String -> Program -> (s, Compile s a)) -> IO ()
compilerMain compile = do
    args <- getArgs
    unless (not $ null args) $ do
        hPutStrLn stderr "Missing inputFileName argument."
        exitFailure
    let inputFileName = head args
    input <- readFile inputFileName
    program <- case pProgram $ myLexer input of
        Bad e -> do
            hPutStrLn stderr e
            exitFailure
        Ok tree -> return tree
    case runCheckMonad (check program) emptyCheckState of
        Left error -> do
            hPutStrLn stderr error
            exitFailure
        otherwise -> return ()
    let (state, compileMonad) = compile inputFileName program
    runCompileMonad compileMonad state
    return ()

