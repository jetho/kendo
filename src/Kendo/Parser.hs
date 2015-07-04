
module Kendo.Parser where

import           Control.Arrow                       ((&&&))
import           Control.Monad.Identity
import           Data.Maybe                          (fromMaybe)
import           Text.Parsec                         hiding (between, parse)
import           Text.Parsec.IndentParsec.Combinator
import           Text.Parsec.IndentParsec.Prim

import qualified Kendo.Lexer                         as L
import           Kendo.Syntax


type Parser a = IndentParsecT String () Identity a


moduleName :: Parser ModuleName
moduleName = (init &&& last) <$> sepBy1 L.upperIdent L.dot

parseModule :: Parser Module
parseModule = do
    Module <$> (L.whiteSpace *> L.reserved "module" *> moduleName)
           <*> (L.reserved "where" *> many parseDecl)

parseDecl :: Parser Decl
parseDecl = choice
    [ parseFunDecl
 --   , parseTypeDecl
 --   , parseDataDecl
 --   , parseClassDecl
 --   , parseInstDecl
 --   , parseFixityDecl
    ]

parseLocalDecl :: Parser Decl
parseLocalDecl = choice
    [ parseFunDecl
    --, parseTypeDecl
    ]

parseFunDecl :: Parser Decl
parseFunDecl = FunDecl <$>
    (BindGroup <$> L.identifier
               <*> (pure <$> parseMatch "=")
               <*> pure Nothing
               <*> (pure <$> parseWhereClause))

parseWhereClause :: Parser [Decl]
parseWhereClause =
    option [] $ L.reserved "where" *> blockOf (many1 parseLocalDecl)

parseMatch :: String -> Parser Match
parseMatch separator =
    Match <$> many parsePattern
          <*> (L.reservedOp separator *> parseExpr)

parsePattern :: Parser Pattern
parsePattern = choice $ map try
    [ parseLitPattern
    , parseVarPattern
    , parseConstrPattern
    , parseWildcardPattern
    ]

parseConstrPattern :: Parser Pattern
parseConstrPattern = PCon <$> L.upperIdent <*> many parsePattern

parseWildcardPattern :: Parser Pattern
parseWildcardPattern = L.underscore *> pure PWild

parseVarPattern :: Parser Pattern
parseVarPattern = PVar <$> L.identifier

parseLitPattern :: Parser Pattern
parseLitPattern = PLit <$> parseLiteral

parseExpr :: Parser Expr
parseExpr = choice
    [ try parseIf
    , try parseLet
    , try parseCase
 --   , parseAnn
 --   , parseDo
 --   , parseFail
    , parseLam
    , parseApp
    ]

parseTerm :: Parser Expr
parseTerm = parseVar <|> parseLit <|> L.parens parseExpr

parseLam :: Parser Expr
parseLam = do
    L.reservedOp "\\"
    args <- many parsePattern
    L.reservedOp "->"
    expr <- parseExpr
    return $ foldr ELam expr args

parseLet :: Parser Expr
parseLet =
    ELet <$> (L.reserved "let" *> L.identifier)
         <*> (L.reserved "="   *> parseExpr)
         <*> (L.reserved "in"  *> parseExpr)

parseIf :: Parser Expr
parseIf =
    EIf <$> (L.reserved "if"   *> parseExpr)
        <*> (L.reserved "then" *> parseExpr)
        <*> (L.reserved "else" *> parseExpr)

parseCase :: Parser Expr
parseCase = ECase
    <$> (L.reserved "case" *> parseApp)
    <*> (L.reserved "of" *> (many1 $ parseMatch "->"))

parseApp :: Parser Expr
parseApp = foldl1 EApp <$> many1 parseTerm

parseVar :: Parser Expr
parseVar = L.lexeme . try $ (EVar <$> L.identifier)

parseLit :: Parser Expr
parseLit = ELit <$> parseLiteral

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

parse :: String -> Either ParseError Module
parse = runIdentity . runGIPT (parseModule <* eof) () ""

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile = fmap parse . readFile

