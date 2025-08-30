module Main where 
import Domain.Instruction (InstructionRepository(loadInstructionSet))
import Domain.Parser (parse)

main :: IO ()
main = do
    commands <- loadInstructionSet "sample01"
    mapM_ (print . parse) commands