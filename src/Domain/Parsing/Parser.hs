module Domain.Parsing.Parser(parse) where

import Domain.Lexer (LexicalAnalysis(..))
import Domain.Model.Lexical
import Domain.Model.Parsing
import qualified Data.HashMap.Strict as HM
import Shared.StringUtils (toBinary15)

parse :: LexicalAnalysis -> Either ParserError [String]
parse LexicalAnalysis { instructions = ins, symbolTable = symTab } =
  traverse (parseInstruction symTab) ins

parseInstruction ::  SymbolTable -> Instruction -> Either ParserError String
parseInstruction symTab i = case i of
  AInstruction a -> 
    case a of
      Variable var -> 
        case HM.lookup var symTab of
          Just aValue -> Right $ toBinaryInstruction aValue
          Nothing     -> Left (ParserError $ "Error Parsing A instruction. Address not found in symbol table. Symbol: " ++ var)
      AbsoluteValue absVal ->
        Right $ toBinaryInstruction absVal

  CInstruction dest comp jmp -> Right $ dest ++ "|" ++ comp ++ "|" ++ jmp
    
toBinaryInstruction :: Int -> String
toBinaryInstruction i = '0' : toBinary15 i