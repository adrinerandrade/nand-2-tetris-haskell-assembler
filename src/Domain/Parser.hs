module Domain.Parser where

import Domain.Lexer (LexicalAnalysis(..))

parse :: LexicalAnalysis -> String
parse LexicalAnalysis { instructions = ins, symbolTable = symTab } =
  "Instructions: " ++ show ins ++ "\nSymbols: " ++ show symTab