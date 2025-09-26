{-# LANGUAGE InstanceSigs #-}
module Infrastructure.Repository (Repository(loadInstructionSet, saveBinaries)) where

import System.FilePath (replaceExtension)

class Monad m => Repository m where
    loadInstructionSet :: String -> m [String]
    saveBinaries :: String -> [String] -> m ()

instance Repository IO where
    loadInstructionSet filePath = do
        fileContent <- readFile filePath
        pure $ lines fileContent

    saveBinaries file binaries = writeFile (replaceExtension file ".bin") (unlines binaries)
