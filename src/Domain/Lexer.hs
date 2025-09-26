{-# LANGUAGE OverloadedStrings #-}
module Domain.Lexer(mapInstructions, LexicalAnalysis(..), LexemeError, SymbolTable) where

import Domain.Model.Lexical
import Domain.Lexemes.Instructions(buildInstruction)
import Shared.CollectionUtils ( distinct );
import qualified Data.HashMap.Strict as HM

data LexicalAnalysis = LexicalAnalysis
  { 
    instructions :: [Instruction],
    symbolTable  :: SymbolTable
  } deriving (Show, Eq)

mapInstructions :: [String] -> Either LexemeError LexicalAnalysis
mapInstructions = _mapInstructions [] [] HM.empty

_mapInstructions :: [Instruction] -> Variables -> LabelTable -> [String] -> Either LexemeError LexicalAnalysis
_mapInstructions ins vars lt [] = Right (LexicalAnalysis (reverse ins) (createSymbolTable (reverse vars) lt))
_mapInstructions ins vars lt (c:cs) =
  case buildInstruction c ins vars lt of
      Left e         -> Left e
      Right (Just i, resultVars, resultingSt) -> _mapInstructions (i:ins) resultVars resultingSt cs
      Right (Nothing, resultVars, resultingSt) -> _mapInstructions ins resultVars resultingSt cs

createSymbolTable :: Variables -> LabelTable -> SymbolTable
createSymbolTable vars = HM.union (varsTable vars)
  where
    varsTable :: Variables -> SymbolTable
    varsTable vs = HM.union (fromVars fixedMappings vs) fixedMappings

    fixedMappings :: SymbolTable
    fixedMappings = 
      HM.fromList [
        ("SCREEN", 16384),
        ("KBD", 24576),
        ("SP", 0),
        ("LCL", 1),
        ("ARG", 2),
        ("THIS", 3),
        ("THAT", 4)
      ]

    fromVars :: SymbolTable -> Variables -> SymbolTable
    fromVars fixedSymbols _vars = HM.fromList $ zip (distinct (getDefaultVariables ++ sanitizedVars fixedSymbols _vars)) [0..]

sanitizedVars :: SymbolTable -> Variables -> Variables
sanitizedVars fixedSymbols = filter (\ v -> not (HM.member v fixedSymbols))

getDefaultVariables :: Variables
getDefaultVariables = [ "R" ++ show i | i <- [0..15 :: Int] ]
