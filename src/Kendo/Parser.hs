
module Kendo.Parser where

import           Control.Arrow      ((&&&))
import           Text.Parsec       
import           Text.Parsec.String 

import qualified Kendo.Lexer        as L
import           Kendo.Syntax


ident :: Parser Char -> Parser String
ident start = L.lexeme ((:) <$> start <*> many identLetter)
  where identLetter = alphaNum <|> oneOf "_'"

upperIdent :: Parser String
upperIdent = ident upper

identifier :: Parser String
identifier = ident $ lower <|> oneOf "_" 

moduleName :: Parser ModuleName
moduleName = (init &&& last) <$> sepBy1 upperIdent L.dot

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

parseFunDecl :: Parser Decl
parseFunDecl = FunDecl <$> 
    (BindGroup <$> identifier
               <*> ((:[]) <$> parseMatch)
               <*> pure Nothing
               <*> pure [])

parseMatch :: Parser Match
parseMatch = 
    Match <$> many parsePattern
          <*> (L.reservedOp "=" *> parseExpr)

parsePattern :: Parser Pattern
parsePattern = choice
    [ PLit <$> parseLiteral
    , PVar <$> identifier
    , parseConstrPattern
    , L.reservedOp "_" *> pure PWild
    ]

parseConstrPattern :: Parser Pattern
parseConstrPattern = PCon <$> upperIdent <*> many parsePattern

parseExpr :: Parser Expr
parseExpr = choice
    [ try parseIf
    , try parseLam
    , try parseLit
 --   , parseLet
    , try parseVar
 --   , parseCase
 --   , parseApp
 --   , parseAnn
 --   , parseDo
 --   , parseFail
    ]

parseVar :: Parser Expr
parseVar = EVar <$> identifier

parseLit :: Parser Expr
parseLit = ELit <$> parseLiteral

parseLam :: Parser Expr
parseLam = do
    L.reservedOp "\\"
    param <- identifier
    L.reservedOp "->"
    expr <- parseExpr 
    return $ ELam param expr

parseLet :: Parser Expr
parseLet = undefined

parseIf :: Parser Expr
parseIf =  
    EIf <$> (L.reserved "if"   *> parseExpr)
        <*> (L.reserved "then" *> parseExpr)
        <*> (L.reserved "else" *> parseExpr)

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

