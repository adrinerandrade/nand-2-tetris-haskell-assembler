{-# LANGUAGE InstanceSigs #-}
module Domain.Instruction (InstructionRepository(loadInstructionSet)) where

import System.FilePath ((</>), (<.>))
import Domain.Lexer(mapInstructions, LexicalAnalysis, LexemeError)

class Monad m => InstructionRepository m where
    loadInstructionSet :: String -> m (Either LexemeError LexicalAnalysis)

instance InstructionRepository IO where
    loadInstructionSet :: String -> IO (Either LexemeError LexicalAnalysis)
    loadInstructionSet name = do
        fileContent <- readFile ("./instructions" </> name <.> "asm")
        pure $ mapInstructions (lines fileContent)
