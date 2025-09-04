module Domain.Model.Lexical where

import qualified Data.HashMap.Strict as HM
import Control.Exception (Exception)

type SymbolTable = HM.HashMap String Int
type LabelTable = HM.HashMap String Int
type Variables = [String]
newtype LexemeError = LexemeError String deriving (Show, Eq)
instance Exception LexemeError

data AValue
    = Variable String
    | AbsoluteValue Int
    deriving (Show, Eq)

data Instruction = 
    AInstruction AValue |
    CInstruction String String String
    deriving (Show, Eq);


