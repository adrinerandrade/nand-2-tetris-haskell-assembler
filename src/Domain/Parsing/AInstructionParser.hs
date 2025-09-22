module Domain.Parsing.AInstructionParser(parseAInstruction) where

import Domain.Model.Lexical
import Domain.Model.Parsing
import qualified Data.HashMap.Strict as HM
import Shared.StringUtils (toBinaryInstruction)

parseAInstruction :: SymbolTable -> AValue -> Either ParserError String
parseAInstruction symTab a = case a of
    Variable var -> 
        case HM.lookup var symTab of
        Just aValue -> Right $ toBinaryInstruction aValue
        Nothing     -> Left (ParserError $ "Error Parsing A instruction. Address not found in symbol table. Symbol: " ++ var)
    AbsoluteValue absVal ->
        Right $ toBinaryInstruction absVal
