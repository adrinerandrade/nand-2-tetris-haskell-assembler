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
createSymbolTable vars lt = HM.union lt (varsTable vars)
  where
    varsTable :: Variables -> SymbolTable
    varsTable = fixedMappings . fromVars

    fixedMappings :: SymbolTable -> SymbolTable
    fixedMappings = HM.insert "SCREEN" 16384 . HM.insert "KEYBOARD" 24576

    fromVars :: Variables -> SymbolTable
    fromVars vs = HM.fromList $ zip (distinct (getDefaultVariables ++ vs)) [0..]

getDefaultVariables :: Variables
getDefaultVariables = [ "R" ++ show i | i <- [0..15 :: Int] ]
