module Luatalk
    ( expr
    ) where

import Text.Parsec
import Text.Parsec.String

{- ident -}

data Ident = Ident String
    deriving (Show, Eq)

ident :: Parser Ident
ident = do
    h <- letter
    t <- many alphaNum
    return $ Ident (h:t)

{- expr ::= atom | binding | unarySend | ( expr ) -}

data Expr
    = ExprAtom Atom
    | ExprBinding Binding
    | ExprParen Expr
    deriving (Show, Eq)

expr :: Parser Expr
expr =  try (ExprBinding <$> binding)
    <|> (ExprAtom <$> atom)
    <|> (ExprParen <$> exprParen)

exprParen :: Parser Expr
exprParen = do
    char '('
    e <- expr
    char ')'
    return e

{- atom ::= ident | literal -}

data Atom
    = AtomIdent Ident
    | AtomLiteral Literal
    deriving (Show, Eq)

atom :: Parser Atom
atom =  (AtomLiteral <$> literal)
    <|> (AtomIdent <$> ident)

{- literal ::= STRING | INTEGER -}

data Literal
    = LiteralString String
    | LiteralInt Int
    deriving (Show, Eq)

literal :: Parser Literal
literal =  (LiteralString <$> literalString)
       <|> (LiteralInt <$> literalInt)

literalString :: Parser String
literalString = do
    char '\''
    s <- many $ noneOf "\'"
    char '\''
    return s

literalInt :: Parser Int
literalInt = do
    n <- many1 digit
    return $ read n

{- binding ::= ident := expr -}

data Binding = Binding Ident Expr
    deriving (Show, Eq)

binding :: Parser Binding
binding = do
    i <- ident
    space
    string ":="
    space
    e <- expr
    return $ Binding i e