{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Domain.Lexer(mapInstructions, Instruction(..), LexicalAnalysis(..), LexemeError, SymbolTable) where

import Shared.StringUtils ( Trim(trim) );
import Shared.CollectionUtils ( distinct );
import Control.Exception (Exception)
import Text.Regex.TDFA ((=~))
import qualified Data.HashMap.Strict as HM

newtype LexemeError = LexemeError String deriving (Show, Eq)

type SymbolTable = HM.HashMap String Int  
type LabelTable = HM.HashMap String Int
type Variables = [String]

data LexicalAnalysis = LexicalAnalysis
  { 
    instructions :: [Instruction],
    symbolTable  :: SymbolTable
  } deriving (Show, Eq)

instance Exception LexemeError

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

buildInstruction :: String -> [Instruction] -> Variables -> LabelTable -> Either LexemeError (Maybe Instruction, Variables, LabelTable)
buildInstruction = identifyInstruction . sanitize

sanitize :: String -> String
sanitize = trim . removeComments

commentChar :: Char
commentChar = '/'

removeComments :: String -> String
removeComments [] = []
removeComments (c:cs)
    | c == commentChar = []
    | otherwise        = c : removeComments cs

data Instruction = 
    AInstruction String |
    CInstruction String |
    Label String
    deriving (Show, Eq);

identifyInstruction :: String -> [Instruction] -> Variables -> LabelTable -> Either LexemeError (Maybe Instruction, Variables, LabelTable)
identifyInstruction [] _ vars st = Right (Nothing, vars, st)
identifyInstruction s@(c:cs) ins vars lt
    | c == '@' = buildAInstruction cs vars lt
    | c == '(' = buildLabel cs ins vars lt
    | otherwise = fmap (, vars, lt) (buildCInstruction s)

buildAInstruction :: String -> Variables -> LabelTable -> Either LexemeError (Maybe Instruction, Variables, LabelTable)
buildAInstruction [] _ _ = Left (LexemeError "Error parsing A instruction. No name after @ found.")
buildAInstruction name vars lt = 
  if name `HM.member` lt then
      Right (Just $ AInstruction name, vars, lt)
  else
      Right (Just $ AInstruction name, name:vars, lt)

buildCInstruction :: String -> Either LexemeError (Maybe Instruction)
buildCInstruction [] = Left (LexemeError "Error parsing C instruction. No name content found.")
buildCInstruction code = Right $ Just (CInstruction code)

-- Labels
labelRegex :: String
labelRegex = "^([a-zA-Z_]+)\\)$"

buildLabel :: String -> [Instruction] -> Variables -> LabelTable -> Either LexemeError (Maybe Instruction, Variables, LabelTable)
buildLabel s ins vars lt = do
    l <- parseLabel s
    if l `HM.member` lt then
      Left (LexemeError $ "Label already exists: " ++ s)
    else
      Right (Nothing, filter (/= l) vars, HM.insert l (length ins) lt)

parseLabel :: String -> Either LexemeError String
parseLabel s =
  case (s =~ labelRegex :: (String, String, String, [String])) of
    (_, _, _, [inner]) -> Right inner
    _                  -> Left (LexemeError $ "Error parsing label. It should be a letters or underscore only encapsulated by parentheshis. Example: (LOOP). Label: " ++ s)