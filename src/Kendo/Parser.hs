
module Kendo.Parser where

import           Control.Arrow      ((&&&))
import           Text.Parsec       
import           Text.Parsec.String 

import qualified Kendo.Lexer        as L
import           Kendo.Syntax


upperIdent :: Parser String
upperIdent = L.lexeme ((:) <$> upper <*> many alphaNum)

moduleName :: Parser ModuleName
moduleName = (init &&& last) <$> sepBy1 upperIdent L.dot

parseModule :: Parser Module
parseModule = do
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
parseFunDecl = do
    fName <- L.identifier
    match <- parseMatch
    return $ FunDecl $ BindGroup fName [match] Nothing []

parseMatch :: Parser Match
parseMatch = do
    pats <- many parsePattern
    L.reservedOp "="
    expr <- parseExpr
    return $ Match pats expr

parsePattern :: Parser Pattern
parsePattern = choice
    [ PLit <$> parseLiteral
    , PVar <$> L.identifier
    , parseConstrPattern
    , L.reservedOp "_" *> pure PWild
    ]

parseConstrPattern :: Parser Pattern
parseConstrPattern = undefined

parseExpr = choice
    [ parseVar
    , parseLam
    , parseLit
    , parseLet
    , parseIf
    , parseCase
    , parseApp
    , parseAnn
    , parseDo
    , parseFail
    ]

parseVar :: Parser Expr
parseVar = EVar <$> L.identifier

parseLit :: Parser Expr
parseLit = ELit <$> parseLiteral

parseLam :: Parser Expr
parseLam = undefined

parseLet :: Parser Expr
parseLet = undefined

parseIf :: Parser Expr
parseIf = undefined

parseCase :: Parser Expr
parseCase = undefined

parseApp :: Parser Expr
parseApp = undefined

parseAnn :: Parser Expr
parseAnn = undefined

parseDo :: Parser Expr
parseDo = undefined

parseFail :: Parser Expr
parseFail = undefined

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

parseLiteral :: Parser Literal
parseLiteral = parseStr <|> parseInt <|> parseChr

parseInt :: Parser Literal
parseInt = LitInt <$> L.integer

parseChr :: Parser Literal
parseChr = LitChar <$> L.chr

parseStr :: Parser Literal
parseStr = LitString <$> L.str

parseString :: String -> Either ParseError Module
parseString s = parse (parseModule <* eof) "" s

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile f = parseFromFile (parseModule <* eof) f

