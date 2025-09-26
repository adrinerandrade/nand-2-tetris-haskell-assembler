module Domain.Parsing.CInstructionParser(parseCInstruction) where

import Domain.Model.Parsing
import Data.Bits ( (.|.) )

parseCInstruction :: String -> String -> String -> Either ParserError String
parseCInstruction dest cmp jmp = fmap (("111" ++) . concat) (sequenceA [translateComputation cmp, translateDest dest, translateJmp jmp])

translateDest :: String -> Either ParserError String
translateDest [] = Right "000"
translateDest dest
    | length dest > 3 = Left (ParserError $ "Invalid destination: " ++ dest) 
    | otherwise     = Right (bitArrToString $ _translateDests dest [0, 0, 0])

bitArrToString :: [Int] -> String
bitArrToString = concatMap show

_translateDests :: String -> [Int] -> [Int]
_translateDests cs acc
  = foldl (\accum c -> orLists accum (_translateDest c)) acc cs

_translateDest :: Char -> [Int]
_translateDest d = case d of
    'M' -> [0, 0, 1]
    'D' -> [0, 1, 0]
    'A' -> [1, 0, 0]
    _   -> [0, 0, 0]

orLists :: [Int] -> [Int] -> [Int]
orLists = zipWith (.|.)

translateComputation :: String -> Either ParserError String
translateComputation "0" = Right "0101010"
translateComputation "1" = Right "0111111"
translateComputation "D" = Right "0001100"
translateComputation "A" = Right "0110000" -- a=0
translateComputation "M" = Right "1110000" -- a=1

-- NumericNegateCmp
translateComputation ('-':cs) = case cs of
  "1" -> Right "0111010"
  "D" -> Right "0001111"
  "A" -> Right "0110011" -- a=0
  "M" -> Right "1110011" -- a=1
  neg  -> Left (ParserError $ "Invalid numeric negate instruction: -" ++ neg)

-- LogicNegateCmp
translateComputation ('!':cs) = case cs of
  "D" -> Right "0001101"
  "A" -> Right "0110001" -- a=0
  "M" -> Right "1110001" -- a=1
  neg  -> Left (ParserError $ "Invalid logic negate instruction: !" ++ neg)

-- D computations
translateComputation s@('D':op) = case op of
  ('+':r) -> case r of
    "1" -> Right "0011111"
    "A" -> Right "0000010" -- a=0
    "M" -> Right "1000010" -- a=1
    _  -> Left (ParserError $ "Invalid sum for D register: -" ++ r)
  ('-':r) -> case r of
    "1" -> Right "0001110"
    "A" -> Right "0010011" -- a=0
    "M" -> Right "1010011" -- a=1
    _  -> Left (ParserError $ "Invalid subtraction for D register: -" ++ r)
  ('&':r) -> case r of
    "A" -> Right "0000000" -- a=0
    "M" -> Right "1000000" -- a=1
    _  -> Left (ParserError $ "Invalid logical AND for D register: -" ++ r)
  ('|':r) -> case r of
    "A" -> Right "0010101" -- a=0
    "M" -> Right "1010101" -- a=1
    _  -> Left (ParserError $ "Invalid logical AND for D register: -" ++ r)

  _  -> Left (ParserError $ "Invalid D operation: " ++ s)

-- A computations
translateComputation s@('A':op) = translateAddressComputation s "A" 0 op

-- M computations
translateComputation s@('M':op) = translateAddressComputation s "M" 1 op

-- error
translateComputation cs     = Left (ParserError $ "Invalid computation instruction: " ++ cs)

translateAddressComputation :: String -> String -> Int -> String -> Either ParserError String
translateAddressComputation s register bitValue op = case op of
  "+1" -> Right $ addressInstruction "110111" 
  ('-':r) -> case r of
    "1" -> Right $ addressInstruction "110010" 
    "D" -> Right $ addressInstruction "000111" 
    _  -> Left (ParserError $ "Invalid subtraction for " ++ register ++ " register: -" ++ r)

  _  -> Left (ParserError $ "Invalid " ++ register ++ " operation: " ++ s)
  where
    addressInstruction :: String -> String
    addressInstruction a = show bitValue ++ a


translateJmp :: String -> Either ParserError String
translateJmp [] = Right "000"
translateJmp "JGT" = Right "001"
translateJmp "JEQ" = Right "010"
translateJmp "JGE" = Right "011"
translateJmp "JLT" = Right "100"
translateJmp "JNE" = Right "101"
translateJmp "JLE" = Right "110"
translateJmp "JMP" = Right "111"
translateJmp cs     = Left (ParserError $ "Invalid jump instruction: " ++ cs)