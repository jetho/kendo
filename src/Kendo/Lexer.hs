
module Kendo.Lexer where

import           Text.Parsec
import           Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token    as Tok


ops   = ["::","..","=","\\","|","<-","->","@","~","=>"]

names = [ "true"
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

lexerConfig = haskellStyle
    { Tok.reservedOpNames = ops
    , Tok.reservedNames   = names
    }

lexer = Tok.makeTokenParser lexerConfig

reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
identifier = Tok.identifier lexer
parens     = Tok.parens lexer
brackets   = Tok.brackets lexer
braces     = Tok.braces lexer
commaSep   = Tok.commaSep lexer
semi       = Tok.semi lexer
integer    = Tok.integer lexer
chr        = Tok.charLiteral lexer
str        = Tok.stringLiteral lexer
operator   = Tok.operator lexer

