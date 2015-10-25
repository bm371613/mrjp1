module CompileToJvm (main) where

import Control.Monad (unless)
import Control.Monad.State (get)
import Data.List (nub)
import Data.Map (fromList, Map, (!))
import Data.Maybe (Maybe(..), mapMaybe)
import System.FilePath.Posix (takeBaseName)

import Bnfc.Absgrammar
import Common (Compile, compilerMain)

-- stack

class StackUser su where
    stackUsage :: su -> Integer

binaryOperationStackUsage :: Exp -> Exp -> Integer
binaryOperationStackUsage e1 e2 = max (stackUsage e1) (stackUsage e2 + 1)

instance StackUser Exp where
    stackUsage (ExpLit _) = 1
    stackUsage (ExpVar _) = 1
    stackUsage (ExpAdd e1 e2) = binaryOperationStackUsage e1 e2
    stackUsage (ExpSub e1 e2) = binaryOperationStackUsage e1 e2
    stackUsage (ExpMul e1 e2) = binaryOperationStackUsage e1 e2
    stackUsage (ExpDiv e1 e2) = binaryOperationStackUsage e1 e2

instance StackUser Stmt where
    stackUsage (SAss _ e) = stackUsage e
    stackUsage (SExp e) = max (stackUsage e) 2

instance StackUser Program where
    stackUsage (Prog stmts) = maximum $ map stackUsage stmts

-- state

data State = State
    { symbolTable :: Map String Integer
    , className :: String
    }

symbolIndex :: String -> Compile State Integer
symbolIndex ident = do
    State st cn <- get
    return $ st ! ident

-- emit

class Emit e where
    emit :: e -> Compile State ()


instance Emit Program where
    emit _ = return () -- TODO

-- main

main :: IO ()
main = compilerMain (\inputFileName program@(Prog stmts) -> 
        (State 
            (fromList $ zip (nub $ mapMaybe assigned stmts) [0..]) 
            (takeBaseName inputFileName),
        emit program))
    where
        assigned :: Stmt -> Maybe String
        assigned (SAss (Ident ident) _) = Just ident
        assigned (SExp _) = Nothing

