module Domain.Lexemes.AInstruction(buildAInstruction) where

import Domain.Model.Lexical

import Text.Regex.TDFA ((=~))

aInstructionRegex :: String
aInstructionRegex = "^(([0-9]+)|([a-zA-Z][a-zA-Z0-9_]*))$"

buildAInstruction :: String -> Either LexemeError Instruction
buildAInstruction str = 
    case (aInstructionRegex =~ str :: (String, String, String, [String])) of
        (_, _, _, [_, absoluteValue]) -> Right (AInstruction $ AbsoluteValue (read absoluteValue :: Int))
        (_, _, _, [_, [], varName]) -> Right (AInstruction $ Variable varName)
        _                  -> Left (LexemeError $ "Invalid AInstruction declaration. It should respect the following pattern: " ++ aInstructionRegex)