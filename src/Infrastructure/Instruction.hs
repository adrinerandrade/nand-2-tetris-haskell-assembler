{-# LANGUAGE InstanceSigs #-}
module Infrastructure.Instruction (InstructionRepository(loadInstructionSet)) where

class Monad m => InstructionRepository m where
    loadInstructionSet :: String -> m [String]

instance InstructionRepository IO where
    loadInstructionSet :: String -> IO [String]
    loadInstructionSet filePath = do
        fileContent <- readFile filePath
        pure $ lines fileContent
