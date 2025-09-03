module Main where 

import System.Environment (getArgs)
import Infrastructure.Repository (Repository(loadInstructionSet))
import Domain.Parser (parse)
import Domain.Lexer (mapInstructions)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [ filePath ] -> do
            l <- loadInstructionSet filePath
            case mapInstructions l of
                Left err -> putStrLn $ "Lexical analysis failed: " ++ show err
                Right lexicalResult -> case parse lexicalResult of
                    Left err -> putStrLn $ "Parsing failed: " ++ show err
                    Right binaries -> print binaries

        _ -> putStrLn "Usage: cabal run nandtotetris-assembler -- <Asm File Path>"