module Domain.Model.Parsing where

newtype ParserError = ParserError String deriving (Show, Eq)