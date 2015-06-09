
module Kendo.Parser where

import           Control.Arrow      ((&&&))
import           Text.Parsec       
import           Text.Parsec.String 

import qualified Kendo.Lexer        as L
import           Kendo.Syntax


moduleName :: Parser ModuleName
moduleName = (init &&& last) <$> sepBy1 moduleIdent L.dot

moduleIdent :: Parser String
moduleIdent = L.lexeme ((:) <$> upper <*> many alphaNum)

moduleParser :: Parser Module
moduleParser = do
    L.whiteSpace
    L.reserved "module"
    name <- moduleName
    L.reserved "where"
    decls <- many parseDecl
    return $ Module name decls

parseDecl :: Parser Decl
parseDecl = choice
    [ parseFunDecl
    , parseTypeDecl
    , parseDataDecl
    , parseClassDecl
    , parseInstDecl
    , parseFixityDecl
    ]

parseFunDecl :: Parser Decl
parseFunDecl = undefined

parseTypeDecl :: Parser Decl
parseTypeDecl = undefined

parseDataDecl :: Parser Decl
parseDataDecl = undefined

parseClassDecl :: Parser Decl
parseClassDecl = undefined

parseInstDecl :: Parser Decl
parseInstDecl = undefined

parseFixityDecl :: Parser Decl
parseFixityDecl = undefined

parseInt :: Parser Literal
parseInt = LitInt <$> L.integer

parseChr :: Parser Literal
parseChr = LitChar <$> L.chr

parseStr :: Parser Literal
parseStr = LitString <$> L.str

parseString :: String -> Either ParseError Module
parseString s = parse (moduleParser <* eof) "" s

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile f = parseFromFile (moduleParser <* eof) f

