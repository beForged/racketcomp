module Main where
import Compile
import Asm.Printer
import System.IO
import System.Environment

--input file, output file name
compileFile :: FilePath -> FilePath -> IO ()
compileFile input output = do
    contents <- readFile input
--dont forget to make a either for parse errors
    writeFile output (asmDisplay (compile (readExpr contents)))

main = do
    (fst:snd)<- getArgs
    --need better than this for sure lol
    _  <- compileFile fst (head snd)
    putStrLn "finished"



