import System.IO (readFile)
import System.Environment

import Grammar.Abs
import Grammar.ErrM
import Grammar.Par

import MemoryTypes
import Interpreter

parse_file :: FilePath -> IO()
parse_file filename = do
    file <- readFile filename
    let 
        tokens = myLexer file
        result = pProgram tokens
    case result of
        Ok (Prog location statements) -> do
            result <- interpret statements
            case result of
                Left err -> putStrLn $ show err
                Right _ -> return ()
        Bad err -> putStrLn ("Parse error: " ++ err)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> parse_file filename
        _ -> putStrLn "Provide a file to interpret."
