module Lexer where

import Prelude hiding (lex)
import Data.Char (isSpace)
import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT

data Token
  = LParen
  | RParen
  | LBracket
  | RBracket
  | Period
  | Pipe
  | Bind
  | Name String
  | Keyword String
  | Arg String
  | Symbol String
  | StringLiteral String
  | CharLiteral Char
  | Number Int
  deriving (Show, Eq)

ppToken :: Token -> String
ppToken LParen            = "("
ppToken RParen            = ")"
ppToken LBracket          = "["
ppToken RBracket          = "]"
ppToken Period            = "."
ppToken Pipe              = "|"
ppToken Bind              = ":="
ppToken (Keyword s)       = s ++ ":"
ppToken (Name s)          = s
ppToken (Arg s)           = ":" ++ s
ppToken (Symbol s)        = s
ppToken (StringLiteral s) = "'" ++ s ++ "'"
ppToken (CharLiteral c)   = "$" ++ [c]
ppToken (Number n)        = show n

data PositionedToken = PositionedToken
  { ptSourcePos :: P.SourcePos
  , ptToken     :: Token
  } deriving (Eq)

instance Show PositionedToken where
  show = ppToken . ptToken

lex :: FilePath -> String -> Either P.ParseError [PositionedToken]
lex filePath input = P.parse parseTokens filePath input

parseTokens :: P.Parsec String u [PositionedToken]
parseTokens = whitespace *> P.many parsePositionedToken <* P.eof

whitespace :: P.Parsec String u ()
whitespace = P.skipMany (P.satisfy isSpace)

parsePositionedToken :: P.Parsec String u PositionedToken
parsePositionedToken = P.try $ do
  pos <- P.getPosition
  tok <- parseToken
  return $ PositionedToken pos tok

parseToken :: P.Parsec String u Token
parseToken = P.choice
  [ P.try $ Bind <$ P.string ":="
  , LParen    <$ P.char '('
  , RParen    <$ P.char ')'
  , LBracket  <$ P.char '['
  , RBracket  <$ P.char ']'
  , Period    <$ P.char '.'
  , Pipe      <$ P.char '|'
  , P.try $ Keyword <$> parseName <* P.char ':'
  , Name <$> parseName
  , Arg <$> (P.char ':' *> parseName)
  , Symbol <$> (P.many1 $ P.oneOf "+-*/")
  , StringLiteral <$> (P.char '\'' *> P.many (P.noneOf "'") <* P.char '\'')
  , CharLiteral <$> (P.char '$' *> P.anyChar)
  , Number . read <$> P.many1 P.digit
  ] <* whitespace

  where
    parseName = (:) <$> nameStart <*> P.many nameLetter
    nameStart = P.letter <|> P.char '_'
    nameLetter = nameStart <|> P.digit

type TokenParser a = P.Parsec [PositionedToken] () a

token :: (Token -> Maybe a) -> TokenParser a
token f = P.token (ppToken . ptToken) ptSourcePos (f . ptToken)

match :: Token -> TokenParser ()
match tok = token (\tok' -> if tok == tok' then Just () else Nothing) P.<?> ppToken tok

parens :: TokenParser a -> TokenParser a
parens p = match LParen *> p <* match RParen


brackets :: TokenParser a -> TokenParser a
brackets p = match LBracket *> p <* match RBracket

charLiteral :: TokenParser Char
charLiteral = token go P.<?> "char literal"
  where go (CharLiteral c) = Just c
        go _               = Nothing

stringLiteral :: TokenParser String
stringLiteral = token go P.<?> "string literal"
  where go (StringLiteral s) = Just s
        go _                 = Nothing

number :: TokenParser Int
number = token go P.<?> "number"
  where go (Number n) = Just n
        go _          = Nothing

identifier :: TokenParser String
identifier = token go P.<?> "identifier"
  where go (Name s) = Just s
        go _        = Nothing

keyword :: TokenParser String
keyword = token go P.<?> "keyword"
  where go (Keyword s) = Just s
        go _           = Nothing

arg :: TokenParser String
arg = token go P.<?> "arg"
  where go (Arg s) = Just s
        go _       = Nothing

symbol :: TokenParser String
symbol = token go P.<?> "symbol"
  where go (Symbol s) = Just s
        go _          = Nothing