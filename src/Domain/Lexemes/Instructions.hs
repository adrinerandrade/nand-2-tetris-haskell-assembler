{-# LANGUAGE TupleSections #-}
module Domain.Lexemes.Instructions(Instruction(..), buildInstruction) where

import Domain.Model.Lexical
import qualified Data.HashMap.Strict as HM
import Shared.StringUtils ( Trim(trim) );
import Domain.Lexemes.CInstruction (buildCInstruction)
import Domain.Lexemes.AInstruction (buildAInstruction)
import Domain.Lexemes.Label(parseLabel)

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


identifyInstruction :: String -> [Instruction] -> Variables -> LabelTable -> Either LexemeError (Maybe Instruction, Variables, LabelTable)
identifyInstruction [] _ vars st = Right (Nothing, vars, st)
identifyInstruction s@(c:cs) ins vars lt
    | c == '@' = processAInstruction cs vars lt
    | c == '(' = processLabel cs ins vars lt
    | otherwise = fmap ((, vars, lt) . Just) (buildCInstruction s)

processAInstruction :: String -> Variables -> LabelTable -> Either LexemeError (Maybe Instruction, Variables, LabelTable)
processAInstruction [] _ _ = Left (LexemeError "Error parsing A instruction. No name after @ found.")
processAInstruction name vars lt = 
  if name `HM.member` lt then
      fmap ((, vars, lt) . Just) (buildAInstruction name)
  else
      fmap ((, name:vars, lt) . Just) (buildAInstruction name)

processLabel :: String -> [Instruction] -> Variables -> LabelTable -> Either LexemeError (Maybe Instruction, Variables, LabelTable)
processLabel s ins vars lt = do
    l <- parseLabel s
    if l `HM.member` lt then
      Left (LexemeError $ "Label already exists: " ++ l)
    else
      Right (Nothing, filter (/= l) vars, HM.insert l (length ins) lt)