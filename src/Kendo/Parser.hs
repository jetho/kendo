
module Kendo.Parser where

import Text.Parsec 
import Text.Parsec.String
import Kendo.Lexer
import Kendo.Syntax


moduleParser :: Parser Module
moduleParser = do
    reserved "module"
    name <- identifier
    reserved "where"
    return $ Module name []

parseString :: String -> Either ParseError Module
parseString s = parse (moduleParser <* eof) "" s

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile f = parseFromFile (moduleParser <* eof) f