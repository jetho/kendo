
module Kendo.Parser where

import           Control.Arrow      ((&&&))
import           Data.Maybe         (fromMaybe)
import qualified Text.Parsec        as P
import           Text.Parsec        ((<|>), ParseError)
import           Text.Parsec.String (Parser, parseFromFile)

import qualified Kendo.Lexer        as L
import           Kendo.Syntax


parens :: Parser a -> Parser a
parens = P.between (P.char '(') (P.char ')')

ident :: Parser Char -> Parser String
ident start = L.lexeme ((:) <$> start <*> P.many identLetter)
  where identLetter = P.alphaNum <|> P.oneOf "_'"

upperIdent :: Parser String
upperIdent = ident P.upper

identifier :: Parser String
identifier = do
    name <- ident $ P.lower <|> (P.oneOf "_" <* P.notFollowedBy L.whiteSpace)
    if name `elem` L.names
    then P.unexpected $ "reserved word " ++ show name
    else return name

moduleName :: Parser ModuleName
moduleName = (init &&& last) <$> P.sepBy1 upperIdent L.dot

parseModule :: Parser Module
parseModule = do
    Module <$> (L.whiteSpace *> L.reserved "module" *> moduleName)
           <*> (L.reserved "where" *> P.many parseDecl)

parseDecl :: Parser Decl
parseDecl = P.choice
    [ parseFunDecl
 --   , parseTypeDecl
 --   , parseDataDecl
 --   , parseClassDecl
 --   , parseInstDecl
 --   , parseFixityDecl
    ]

parseLocalDecl :: Parser Decl
parseLocalDecl = P.choice
    [ parseFunDecl
    --, parseTypeDecl
    ]

parseFunDecl :: Parser Decl
parseFunDecl = FunDecl <$> 
    (BindGroup <$> identifier
               <*> (pure <$> parseMatch)
               <*> pure Nothing
               <*> parseWhereClause)

parseWhereClause :: Parser [[Decl]]
parseWhereClause = pure <$> 
    (fromMaybe [] <$> P.optionMaybe (L.reserved "where" *> P.many1 parseLocalDecl))

parseMatch :: Parser Match
parseMatch = 
    Match <$> P.many parsePattern
          <*> (L.reservedOp "=" *> parseExpr)

parsePattern :: Parser Pattern
parsePattern = P.choice $ map P.try 
    [ parseLitPattern
    , parseVarPattern
    , parseConstrPattern
    , parseWildcardPattern
    ]

parseConstrPattern :: Parser Pattern
parseConstrPattern = PCon <$> upperIdent <*> P.many parsePattern

parseWildcardPattern :: Parser Pattern
parseWildcardPattern = L.reservedOp "_" *> pure PWild

parseVarPattern :: Parser Pattern
parseVarPattern = PVar <$> identifier

parseLitPattern :: Parser Pattern
parseLitPattern = PLit <$> parseLiteral

parseExpr :: Parser Expr
parseExpr = P.choice 
    [ P.try parseIf
    , P.try parseLet
    , parseLam
 --   , parseCase
 --   , parseAnn
 --   , parseDo
 --   , parseFail
    , parseApp
    ]

parseTerm :: Parser Expr
parseTerm = parseVar <|> parseLit <|> parens parseExpr

parseLam :: Parser Expr
parseLam = do
    L.reservedOp "\\"
    args <- P.many parsePattern
    L.reservedOp "->"
    expr <- parseExpr 
    return $ foldr ELam expr args

parseLet :: Parser Expr
parseLet = 
    ELet <$> (L.reserved "let" *> identifier)
         <*> (L.reserved "="   *> parseExpr)
         <*> (L.reserved "in"  *> parseExpr)

parseIf :: Parser Expr
parseIf =  
    EIf <$> (L.reserved "if"   *> parseExpr)
        <*> (L.reserved "then" *> parseExpr)
        <*> (L.reserved "else" *> parseExpr)

parseCase :: Parser Expr
parseCase = undefined 

parseApp :: Parser Expr
parseApp = foldl1 EApp <$> P.many1 parseTerm

parseVar :: Parser Expr
parseVar = L.lexeme . P.try $ (EVar <$> identifier)

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

parseString :: String -> Either ParseError Module
parseString = P.parse (parseModule <* P.eof) "" 

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile = parseFromFile (parseModule <* P.eof) 

