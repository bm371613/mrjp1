module CompileToJvm (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.List (nub)
import Data.Map (fold, fromList, Map, (!))
import Data.Maybe (Maybe(..), mapMaybe)
import System.FilePath.Posix (takeBaseName)
import Text.Printf (printf)

import Bnfc.Absgrammar
import Common (Compile, compilerMain)

-- stack

class StackUser su where
    stackUsage :: su -> Integer

binaryOperationStackUsage :: Exp -> Exp -> Integer
binaryOperationStackUsage e1 e2 = max (max u1 u2) (1 + min u1 u2)
    where
        u1 = stackUsage e1
        u2 = stackUsage e2

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

emitBinaryOperation :: Bool -> String -> Exp -> Exp -> Compile State ()
emitBinaryOperation commutative op e1 e2 = do
    if stackUsage e1 >= stackUsage e2
    then do
        emit e1
        emit e2
    else do
        emit e2
        emit e1
        unless commutative $ liftIO $ putStrLn "    swap"
    liftIO $ putStrLn $ printf "    %s" op

instance Emit Exp where
    emit (ExpLit l) = liftIO $ putStrLn $ printf "    %s" instr
        where instr
                | l == -1                   = "iconst_m1" 
                | 0 <= l && l <= 5          = printf "iconst_%d" l
                | -128 <= l && l <= 127     = printf "bipush %d" l
                | otherwise                 = printf "ldc %d" l
    emit (ExpVar (Ident ident)) = do
        ix <- symbolIndex ident
        liftIO $ putStrLn $ printf "    iload %d" ix
    emit (ExpAdd e1 e2) = emitBinaryOperation True "iadd" e1 e2
    emit (ExpSub e1 e2) = emitBinaryOperation False "isub" e1 e2
    emit (ExpMul e1 e2) = emitBinaryOperation True "imul" e1 e2
    emit (ExpDiv e1 e2) = emitBinaryOperation False "idiv" e1 e2

instance Emit Stmt where
    emit (SAss (Ident ident) e) = do
        emit e
        ix <- symbolIndex ident
        liftIO $ putStrLn $ printf "    istore %d" ix
    emit (SExp e) = if stackUsage e < 2
        then do
            liftIO $ putStrLn
                "    getstatic java/lang/System/out Ljava/io/PrintStream;"
            emit e
            liftIO $ putStrLn
                "    invokevirtual java/io/PrintStream/println(I)V"
        else do
            emit e
            liftIO $ putStrLn
                "    getstatic java/lang/System/out Ljava/io/PrintStream;"
            liftIO $ putStrLn
                "    swap"
            liftIO $ putStrLn
                "    invokevirtual java/io/PrintStream/println(I)V"

instance Emit Program where
    emit program@(Prog stmts) = do
        (State st cn) <- get
        liftIO $ putStr $ printf "\
            \.class  public %s\n\
            \.super  java/lang/Object\n\
            \\n\
            \.method public <init>()V\n\
            \   aload_0\n\
            \   invokespecial java/lang/Object/<init>()V\n\
            \   return\n\
            \.end method\n\
            \\n\
            \.method public static main([Ljava/lang/String;)V\n\
            \.limit stack %d\n\
            \.limit locals %d\n" cn (stackUsage program) (1 + fold max 0 st)
        mapM_ emit stmts
        liftIO $ putStr "\
            \    return\n\
            \.end method\n"

-- main

main :: IO ()
main = compilerMain (\inputFileName program@(Prog stmts) -> 
        (State 
            (fromList $ zip (nub $ mapMaybe assigned stmts) [1..])
            (takeBaseName inputFileName),
        emit program))
    where
        assigned :: Stmt -> Maybe String
        assigned (SAss (Ident ident) _) = Just ident
        assigned (SExp _) = Nothing

