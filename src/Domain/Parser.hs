module Domain.Parser () where
import Domain.Lexer (LexicalAnalysis)

parse :: LexicalAnalysis -> String
parse = show