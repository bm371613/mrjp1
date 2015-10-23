module CompileToLlvm (main) where

import MainUtils

main :: IO ()
main = compilerMain (\_ -> \_ -> return ()) ()

