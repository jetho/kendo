
module Kendo.Lexer where

import           Control.Monad.Identity
import           Text.Parsec
import           Text.Parsec.IndentParsec.Prim
import qualified Text.Parsec.IndentParsec.Token as I
import qualified Text.Parsec.Token              as P


type Parser a = IndentParsecT String () Identity a


reservedNames = 
    [ "true"
    , "false"
    , "data"
    , "type"
    , "import"
    , "infixl"
    , "infixr"
    , "infix"
    , "class"
    , "instance"
    , "module"
    , "case"
    , "of"
    , "if"
    , "then"
    , "else"
    , "do"
    , "let"
    , "in"
    , "where"
    , "infixl"
    , "infixr"
    , "infix"
    ]

reservedOps = 
    [ "::"
    , ".."
    , "="
    , "\\"
    , "|"
    , "<-"
    , "->"
    , "@"
    , "~"
    , "=>"
    ]


identLetter = alphaNum <|> oneOf "_'"
identStart  = letter <|> char '_'
opLetter    = oneOf ":!#$%&*+./<=>?@\\^|-~"


lexerConfig :: P.GenLanguageDef String u (IndentT HaskellLike Identity)
lexerConfig = P.LanguageDef
    { P.commentStart = ""
    , P.commentEnd = ""
    , P.commentLine = "--"
    , P.nestedComments = False
    , P.identStart = identStart
    , P.identLetter = identLetter
    , P.opStart = P.opLetter lexerConfig
    , P.opLetter = opLetter
    , P.reservedOpNames = reservedOps
    , P.reservedNames = reservedNames
    , P.caseSensitive = True
    }

lexer :: I.IndentTokenParser String () Identity
lexer      = P.makeTokenParser lexerConfig

reserved   = I.reserved lexer
reservedOp = I.reservedOp lexer
parens     = I.parens lexer
brackets   = I.brackets lexer
braces     = I.braces lexer
commaSep   = I.commaSep lexer
commaSep1  = I.commaSep1 lexer
semi       = I.semi lexer
integer    = I.integer lexer
chr        = I.charLiteral lexer
str        = I.stringLiteral lexer
operator   = I.operator lexer
dot        = I.dot lexer
whiteSpace = I.whiteSpace lexer
lexeme     = I.lexeme lexer

underscore :: Parser ()
underscore = reserved "_"

ident :: Parser Char -> Parser String
ident start = lexeme ((:) <$> start <*> many identLetter)

upperIdent :: Parser String
upperIdent = ident upper

identifier :: Parser String
identifier = do
    name <- ident $ lower <|> (oneOf "_" <* notFollowedBy whiteSpace)
    if name `elem` reservedNames
    then unexpected $ "reserved word " ++ show name
    else return name

