module Domain.Parsing.CInstructionParser(parseCInstruction) where

import Domain.Model.Parsing
import Data.Bits ( (.|.) )

parseCInstruction :: String -> String -> String -> Either ParserError String
parseCInstruction dest cmp jmp = fmap concat (sequenceA [translateDest dest, translateComputation cmp, translateJmp jmp])

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
translateComputation "0" = Right "0101010" -- a=0
translateComputation "1" = Right "0111111" -- a=0
translateComputation "D" = Right "0001100" -- a=0
translateComputation "A" = Right "0110000" -- a=0
translateComputation "M" = Right "1110000" -- a=1
translateComputation ('-':cs) = case cs of
  "1" -> Right "0111010" -- a=0
  "D" -> Right "0001111" -- a=0
  "A" -> Right "0110011" -- a=0
  "M" -> Right "1110011" -- a=1
  neg  -> Left (ParserError $ "Invalid negate instruction: -" ++ neg)

-- error
translateComputation cs     = Left (ParserError $ "Invalid computation instruction: " ++ cs)

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