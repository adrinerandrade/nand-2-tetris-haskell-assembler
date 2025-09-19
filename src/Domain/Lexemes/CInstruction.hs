module Domain.Lexemes.CInstruction(buildCInstruction) where

import Domain.Model.Lexical
import Text.Regex.TDFA ((=~))

cInstructionRegex :: String
cInstructionRegex = "^(([ADM][ADM]?[ADM]?)=)?([ADM\\+\\-\\|&!01]+)(;([JNLEQGTMP][JNLEQGTMP][JNLEQGTMP]))?$"

buildCInstruction :: String -> Either LexemeError Instruction
buildCInstruction text = case text =~ cInstructionRegex :: (String, String, String, [String]) of
    (_, _, _, [_, dest, comp, _, jump]) -> 
        Right (CInstruction dest comp jump)
    _ ->
        Left (LexemeError $ "Invalid C-Instruction declaration. It should respect the following pattern: " ++ cInstructionRegex ++ ". Instruction: " ++ text)