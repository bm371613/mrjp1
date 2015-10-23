module MainUtils where

import Control.Monad (unless)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Bnfc.Absgrammar
import Bnfc.Lexgrammar
import Bnfc.Pargrammar
import Bnfc.ErrM
import Compile
import Check

compilerMain :: (String -> Program -> Compile s ()) -> s -> IO ()
compilerMain compile state = do
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
    runCompileMonad (compile inputFileName program) state

