module Domain.Parsing.CInstructionParser where

import Domain.Model.Parsing
import Data.Bits ( (.|.) )

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
