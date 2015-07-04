
module Kendo.Lexer where

import           Control.Monad.State
import           Text.Parsec         hiding (State)
import           Text.Parsec.Indent
import           Text.Parsec.Pos     (SourcePos)
import qualified Text.Parsec.Token   as P


type Parser a = IndentParser String () a


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


identLetter, identStart, opLetter :: Parser Char
identLetter = alphaNum <|> oneOf "_'"
identStart  = letter <|> char '_'
opLetter    = oneOf ":!#$%&*+./<=>?@\\^|-~"


lexerConfig :: P.GenLanguageDef String () (State SourcePos)
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

lexer :: P.GenTokenParser String () (State SourcePos)
lexer      = P.makeTokenParser lexerConfig

reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
parens     = P.parens lexer
brackets   = P.brackets lexer
braces     = P.braces lexer
commaSep   = P.commaSep lexer
commaSep1  = P.commaSep1 lexer
semi       = P.semi lexer
integer    = P.integer lexer
chr        = P.charLiteral lexer
str        = P.stringLiteral lexer
operator   = P.operator lexer
dot        = P.dot lexer
whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer

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

