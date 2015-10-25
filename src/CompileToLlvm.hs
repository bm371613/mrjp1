module CompileToLlvm (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, state)
import Data.Map (Map, empty, insert, (!))
import Text.Printf (printf)

import Bnfc.Absgrammar
import Common (Compile, compilerMain)

-- result

data Result = Registry String | Literal Integer | None

instance Show Result where
    show (Registry registry) = registry
    show (Literal integer) = show integer

-- state

data State = State
    { symbolTable :: Map String Result
    , nextRegistryNumber :: Int
    }

nextRegistry :: Compile State String
nextRegistry = do   
    registryNumber <- state (\(State st nrn) -> (nrn, State st $ nrn + 1))
    return $ printf "%%r%d" registryNumber

-- emit

class Emit e where
    emit :: e -> Compile State Result

emitBinaryOperation :: String -> Exp -> Exp -> Compile State Result
emitBinaryOperation op e1 e2 = do
    r1 <- emit e1
    r2 <- emit e2
    registry <- nextRegistry
    liftIO $ putStrLn
        $ printf "    %s = %s i32 %s, %s" registry op (show r1) (show r2)
    return $ Registry registry

instance Emit Exp where
    emit (ExpLit l) = return $ Literal l
    emit (ExpVar (Ident ident)) = do
        state <- get
        return $ symbolTable state ! ident
    emit (ExpAdd e1 e2) = emitBinaryOperation "add" e1 e2
    emit (ExpSub e1 e2) = emitBinaryOperation "sub" e1 e2
    emit (ExpMul e1 e2) = emitBinaryOperation "mul" e1 e2
    emit (ExpDiv e1 e2) = emitBinaryOperation "sdiv" e1 e2

instance Emit Stmt where
    emit (SAss (Ident ident) e) = do
        r <- emit e
        modify $ \(State st nrn) -> State (insert ident r st) nrn
        return None
    emit (SExp e) = do
        r <- emit e
        liftIO $ putStrLn $ printf "    call void @printInt(i32 %s)" (show r)
        return None

instance Emit Program where
    emit (Prog stmts) = do
        liftIO $ putStr "\
            \@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n\
            \\n\
            \declare i32 @printf(i8*, ...)\n\
            \\n\
            \define void @printInt(i32 %x) {\n\
            \entry: %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0\n\
            \    call i32 (i8*, ...)* @printf(i8* %t0, i32 %x)\n\
            \    ret void\n\
            \}\n\
            \\n\
            \define i32 @main() {\n"
        mapM_ emit stmts
        liftIO $ putStr "\
            \    ret i32 0\n\
            \}\n"
        return None

-- main

main :: IO ()
main = compilerMain (\_ program -> (State empty 0, emit program))

