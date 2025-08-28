module Main where 
import Domain.Instruction (InstructionRepository(loadInstructionSet))

main :: IO ()
main = do
    commands <- loadInstructionSet "sample01"
    mapM_ print commands