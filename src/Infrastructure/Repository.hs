{-# LANGUAGE InstanceSigs #-}
module Infrastructure.Repository (Repository(loadInstructionSet)) where

class Monad m => Repository m where
    loadInstructionSet :: String -> m [String]

instance Repository IO where
    loadInstructionSet :: String -> IO [String]
    loadInstructionSet filePath = do
        fileContent <- readFile filePath
        pure $ lines fileContent
