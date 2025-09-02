module Domain.Parser(parse) where

import Domain.Lexer (LexicalAnalysis(..), SymbolTable, Instruction(..))
import qualified Data.HashMap.Strict as HM
import Shared.StringUtils (toBinary15)

newtype ParserError = ParserError String deriving (Show, Eq)

parse :: LexicalAnalysis -> Either ParserError [String]
parse LexicalAnalysis { instructions = ins, symbolTable = symTab } =
  traverse (parseInstruction symTab) ins

parseInstruction ::  SymbolTable -> Instruction -> Either ParserError String
parseInstruction symTab i = case i of
  AInstruction a -> 
      case HM.lookup a symTab of
        -- Just aValue -> Right ('0' : toBinary15 aValue)
        Just aValue -> Right ('0' : show aValue)
        Nothing     -> Left (ParserError $ "Error Parsing A instruction. Address not found in symbol table. Symbol: " ++ a)
        
  CInstruction c -> Right c
  Label        l -> Left (ParserError $ ". Label: " ++ l)