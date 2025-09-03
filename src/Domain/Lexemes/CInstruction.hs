module Domain.Lexemes.CInstruction(buildCInstruction) where

import Domain.Model.Lexical
import Text.Regex.TDFA ((=~))

cInstructionRegex :: String
cInstructionRegex = "^(([A-Z]+)=)?([A-Z+-|&!01]+)(;([A-Z]+))?$"

buildCInstruction :: String -> Either LexemeError Instruction
buildCInstruction text = Right (CInstruction text text text)