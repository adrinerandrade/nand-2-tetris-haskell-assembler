module Domain.Lexemes.Label(parseLabel) where

import Domain.Model.Lexical

import Text.Regex.TDFA ((=~))

labelRegex :: String
labelRegex = "^([a-zA-Z_]+)\\)$"

parseLabel :: String -> Either LexemeError String
parseLabel s =
  case (s =~ labelRegex :: (String, String, String, [String])) of
    (_, _, _, [inner]) -> Right inner

    _ ->
      Left (LexemeError $ "Error parsing label. It should be a letters or underscore only encapsulated by parentheshis. Example: (LOOP). Label: " ++ s)