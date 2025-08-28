{-# LANGUAGE OverloadedStrings #-}
module Domain.Lexer(mapInstructions, Instruction(..), LexemeError) where

import Shared.StringUtils ( Trim(trim) );
import Control.Exception (Exception)
import Text.Regex.TDFA ((=~))

newtype LexemeError = LexemeError String
  deriving (Show, Eq)

instance Exception LexemeError

mapInstructions :: [String] -> Either LexemeError [Instruction]
mapInstructions = _mapInstructions []

_mapInstructions :: [Instruction] -> [String] -> Either LexemeError [Instruction]
_mapInstructions ins [] = Right (reverse ins)
_mapInstructions ins (c:cs) =
  case buildInstruction c of
      Left e         -> Left e
      Right (Just i) -> _mapInstructions (i:ins) cs
      Right Nothing  -> _mapInstructions ins cs

buildInstruction :: String -> Either LexemeError (Maybe Instruction)
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
    Symbol String
    deriving (Show, Eq);

identifyInstruction :: String -> Either LexemeError (Maybe Instruction)
identifyInstruction [] = Right Nothing
identifyInstruction s@(c:cs)
    | c == '@' = buildAInstruction cs
    | c == '(' = buildSymbol cs
    | otherwise = buildCInstruction s

buildAInstruction :: String -> Either LexemeError (Maybe Instruction)
buildAInstruction [] = Left (LexemeError "Error parsing A instruction. No name after @ found.")
buildAInstruction name = Right $ Just (AInstruction name)

buildCInstruction :: String -> Either LexemeError (Maybe Instruction)
buildCInstruction [] = Left (LexemeError "Error parsing C instruction. No name content found.")
buildCInstruction code = Right $ Just (CInstruction code)

-- Symbols
symbolRegex :: String
symbolRegex = "^([a-zA-Z_]+)\\)$"

buildSymbol :: String -> Either LexemeError (Maybe Instruction)
buildSymbol s = fmap (Just . Symbol) (parseSymbol s)

parseSymbol :: String -> Either LexemeError String
parseSymbol s =
  case (s =~ symbolRegex :: (String, String, String, [String])) of
    (_, _, _, [inner]) -> Right inner
    _                  -> Left (LexemeError ("Error parsing symbol. It should be a letters or underscore only encapsulated by parentheshis. Example: (LOOP). Symbol: " ++ s))