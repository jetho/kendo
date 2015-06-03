
module Kendo.Parser where

import           Control.Arrow      ((&&&))
import           Text.Parsec
import           Text.Parsec.String

import           Kendo.Lexer
import           Kendo.Syntax


moduleName :: Parser ModuleName
moduleName = (init &&& last) <$> sepBy1 identifier dot

moduleParser :: Parser Module
moduleParser = do
    reserved "module"
    name <- moduleName
    reserved "where"
    return $ Module name []

parseString :: String -> Either ParseError Module
parseString s = parse (moduleParser <* eof) "" s

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile f = parseFromFile (moduleParser <* eof) f
