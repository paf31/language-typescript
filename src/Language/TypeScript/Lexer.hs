-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Lexer
-- Copyright   :  (c) DICOM Grid Inc. 2013
-- License     :  MIT
--
-- Maintainer  :  Phillip Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.TypeScript.Lexer (
  identifier
  , reserved
  , operator
  , reservedOp
  , charLiteral
  , stringLiteral
  , natural
  , integer
  , float
  , naturalOrFloat
  , decimal
  , hexadecimal
  , octal
  , symbol
  , lexeme
  , whiteSpace
  , parens
  , braces
  , angles
  , brackets
  , squares
  , semi
  , comma
  , colon
  , dot
  , semiSep
  , semiSep1
  , commaSep
  , commaSep1
) where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

typeScriptDef =	javaStyle
  { T.identStart = oneOf "_$" <|> letter
  , T.reservedNames = [
      "break", "do", "instanceof", "typeof", "case", "else", "new", "var", "catch", "finally", "return", "void", "continue", "for",
      "switch", "while", "debugger", "function", "this", "with", "default", "if", "throw", "delete", "in", "try", "class", "enum",
      "extends", "super", "const", "export", "import", "implements", "let", "private", "public", "yield", "interface", "package",
      "protected", "static"
    ]
  , T.caseSensitive = True
  }

parser                = T.makeTokenParser typeScriptDef

identifier            = T.identifier parser
reserved              = T.reserved parser
operator              = T.operator parser
reservedOp            = T.reservedOp parser
charLiteral           = T.charLiteral parser
stringLiteral         = T.stringLiteral parser
natural               = T.natural parser
integer               = T.integer parser
float                 = T.float parser
naturalOrFloat        = T.naturalOrFloat parser
decimal               = T.decimal parser
hexadecimal           = T.hexadecimal parser
octal                 = T.octal parser
symbol                = T.symbol parser
lexeme                = T.lexeme parser
whiteSpace            = T.whiteSpace parser
parens                = T.parens parser
braces                = T.braces parser
angles                = T.angles parser
brackets              = T.brackets parser
squares               = T.brackets parser
semi                  = T.semi parser
comma                 = T.comma parser
colon                 = T.colon parser
dot                   = T.dot parser
semiSep               = T.semiSep parser
semiSep1              = T.semiSep1 parser
commaSep              = T.commaSep parser
commaSep1             = T.commaSep1 parser
