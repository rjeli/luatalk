module Parser where

import Text.Parsec as P

import Lexer

data Statement = Statement [Expression] (Maybe Expression)
  deriving (Show, Eq)

parseStatement :: TokenParser Statement
parseStatement = do -- Statement <$> (try $ parseExpression `P.endBy` (match Period)) <*> (try $ P.optionMaybe parseExpression)-- <$> (P.optionMaybe parseExpression)
  h <- P.many . try $ parseExpression <* (match Period)
  t <- if (null h) then Just <$> parseExpression
          else try $ P.optionMaybe parseExpression
  return $ Statement h t
  
data Expression
  = ExpressionBind Identifier Expression
  | ExpressionSend Operand [UnaryMessage] [BinaryMessage] [KeywordMessage]
  deriving (Show, Eq)

parseExpression :: TokenParser Expression
parseExpression = parseExpressionBind <|> parseExpressionSend
  where
    parseExpressionBind = 
      ExpressionBind <$> (P.try $ identifier <* match Bind) <*> parseExpression

    parseExpressionSend = do
      operand <- parseOperand
      uMessages <- P.many parseUnaryMessage
      binMessages <- P.many parseBinaryMessage
      kwMessages <- P.many parseKeywordMessage
      return $ ExpressionSend operand uMessages binMessages kwMessages

data Operand
  = OperandIdentifier Identifier
  | OperandLiteral Literal
  | OperandBlock Block
  | OperandNested Expression
  deriving (Show, Eq)

parseOperand :: TokenParser Operand
parseOperand = (OperandIdentifier <$> identifier)
           <|> (OperandLiteral <$> parseLiteral)
           <|> (OperandBlock <$> parseBlock)
           <|> (OperandNested <$> parens parseExpression)

data Literal
  = LiteralString String
  | LiteralNumber Int
  deriving (Show, Eq)

parseLiteral :: TokenParser Literal
parseLiteral = (LiteralString <$> stringLiteral)
           <|> (LiteralNumber <$> number)

data Block = Block [Identifier] [Statement]
  deriving (Show, Eq)

parseBlock = try parseBlockWithArgs
         <|> parseBlockWithoutArgs

parseBlockWithArgs = brackets $ do
  args <- P.many1 arg
  match Pipe
  body <- P.many parseStatement
  return $ Block args body

parseBlockWithoutArgs = Block [] <$> brackets (P.many parseStatement)

newtype UnaryMessage = UnaryMessage Identifier
  deriving (Show, Eq)

parseUnaryMessage :: TokenParser UnaryMessage
parseUnaryMessage = UnaryMessage <$> identifier

data BinaryMessage = BinaryMessage String Operand
  deriving (Show, Eq)

parseBinaryMessage :: TokenParser BinaryMessage
parseBinaryMessage = BinaryMessage <$> symbol <*> parseOperand

data KeywordMessage = KeywordMessage Identifier Operand
  deriving (Show, Eq)

parseKeywordMessage :: TokenParser KeywordMessage
parseKeywordMessage = KeywordMessage <$> keyword <*> parseOperand

type Identifier = String