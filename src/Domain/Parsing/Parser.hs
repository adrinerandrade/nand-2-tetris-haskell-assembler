module Domain.Parsing.Parser(parse) where

import Domain.Lexer (LexicalAnalysis(..))
import Domain.Model.Lexical
import Domain.Model.Parsing
import Domain.Parsing.AInstructionParser(parseAInstruction)
import Domain.Parsing.CInstructionParser(parseCInstruction)

parse :: LexicalAnalysis -> Either ParserError [String]
parse LexicalAnalysis { instructions = ins, symbolTable = symTab } =
  traverse (parseInstruction symTab) ins

parseInstruction ::  SymbolTable -> Instruction -> Either ParserError String
parseInstruction symTab i = case i of
  AInstruction a ->
    parseAInstruction symTab a

  CInstruction dest cmp jmp ->
    parseCInstruction dest cmp jmp
