{-# LANGUAGE InstanceSigs #-}
module Domain.Instruction (InstructionRepository(loadInstructionSet)) where

import System.FilePath ((</>), (<.>))
import Domain.Lexer(mapInstructions, Instruction(..), LexemeError)

class Monad m => InstructionRepository m where
    loadInstructionSet :: String -> m (Either LexemeError [Instruction])

instance InstructionRepository IO where
    loadInstructionSet :: String -> IO (Either LexemeError [Instruction])
    loadInstructionSet name = do
        fileContent <- readFile ("./instructions" </> name <.> "asm")
        pure $ mapInstructions (lines fileContent)
