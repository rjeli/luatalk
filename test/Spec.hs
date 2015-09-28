import Data.Either

import Test.Hspec
import qualified Text.Parsec as P

import Lib
import Lexer as L
import Parser as P

program = unlines
  [ "dong inflate."
  , "[ 3 ] value."
  , "[ :x | x name ] value: 9."
  ]

lexerSpec = do

  describe "parseToken" $ do

    let parseToken = P.parse L.parseToken "(spec)"

    it "parses LBracket" $ do
      parseToken "[" `shouldBe` Right LBracket

    it "parses RBracket" $ do
      parseToken "]" `shouldBe` Right RBracket

    it "parses Period" $ do
      parseToken "." `shouldBe` Right Period

    it "parses Pipe" $ do
      parseToken "|" `shouldBe` Right Pipe

    it "parses Bind" $ do
      parseToken ":=" `shouldBe` Right Bind

    it "parses Name" $ do
      parseToken "abc" `shouldBe` Right (Name "abc")

    it "parses a Name with a number" $ do
      parseToken "abc1" `shouldBe` Right (Name "abc1")

    it "parses Keyword" $ do
      parseToken "abc:" `shouldBe` Right (Keyword "abc")

    it "parses Arg" $ do
      parseToken ":abc" `shouldBe` Right (Arg "abc")

    it "parses Symbol" $ do
      parseToken "+" `shouldBe` Right (Symbol "+")

    it "parses StringLiteral" $ do
      parseToken "'hello'" `shouldBe` Right (StringLiteral "hello")

    it "parses CharLiteral" $ do
      parseToken "$a" `shouldBe` Right (CharLiteral 'a')

    it "parses Number" $ do
      parseToken "32" `shouldBe` Right (Number 32)

parserSpec = do

  let parse p s = do
        lexed <- P.parse L.parseTokens "(spec)" s
        P.parse p "(spec)" lexed 

  describe "parseLiteral" $ do

    let parseLiteral = parse P.parseLiteral 

    it "parses string literal" $ do
      parseLiteral "'hi'" `shouldBe` Right (LiteralString "hi")

    it "parses number literal" $ do
      parseLiteral "23" `shouldBe` Right (LiteralNumber 23)

  describe "parseStatement" $ do

    let parseStatement = parse P.parseStatement

    it "parses unary message" $ do
      parseStatement "abc def." `shouldBe` 
        Right (Statement [(ExpressionSend (OperandIdentifier "abc") [UnaryMessage "def"] [] [])] Nothing)

    it "parses binary message" $ do
      parseStatement "abc + def." `shouldBe` 
        Right (Statement [(ExpressionSend (OperandIdentifier "abc") 
                                          [] 
                                          [BinaryMessage "+" (OperandIdentifier "def")] 
                                          [])] Nothing)

    it "parses keyword message" $ do
      parseStatement "abc def: xyz." `shouldBe`
        Right (Statement [(ExpressionSend (OperandIdentifier "abc") 
                                          [] 
                                          [] 
                                          [KeywordMessage "def" (OperandIdentifier "xyz")])] Nothing)

    it "parses combined message" $ do
      parseStatement "a b - c d: e." `shouldBe`
        Right (Statement [(ExpressionSend (OperandIdentifier "a")
                                          [UnaryMessage "b"]
                                          [BinaryMessage "-" (OperandIdentifier "c")]
                                          [KeywordMessage "d" (OperandIdentifier "e")])] Nothing)

    it "parses message to nested expression" $ do
      parseStatement "(a b) c." `shouldBe`
        Right (Statement [(ExpressionSend (OperandNested (ExpressionSend (OperandIdentifier "a") [UnaryMessage "b"] [] []))
                                          [UnaryMessage "c"]
                                          []
                                          [])] Nothing)

    it "parses a naked expression as a statement" $ do
      parseStatement "3" `shouldBe` Right (Statement [] (Just $ ExpressionSend (OperandLiteral (LiteralNumber 3)) [] [] []))

    it "parses block without args" $ do
      parseStatement "[ 3 ]" `shouldBe` 
        Right (Statement [] (Just $ (ExpressionSend 
                                      (OperandBlock 
                                        (Block [] [Statement [] (Just $ ExpressionSend (OperandLiteral (LiteralNumber 3)) [] [] [])])) 
                                        [] [] []
                                      )
                                    ))

    it "parses block with args" $ do
      parseStatement "[ :x | x ]" `shouldBe`
        Right (Statement [] (Just $ (ExpressionSend
                                     (OperandBlock
                                       (Block ["x"] [Statement [] (Just $ ExpressionSend (OperandIdentifier "x") [] [] [])])
                                     ) [] [] []
                                   )))

    it "parses block with multiple args and statements" $ do
      parseStatement "[ :x :y | 2 negate. x + y / 2 ]" `shouldBe`
        Right (Statement [] (Just $ ExpressionSend
            (OperandBlock 
              (Block ["x", "y"] 
                [Statement 
                  [ExpressionSend (OperandLiteral (LiteralNumber 2)) [UnaryMessage "negate"] [] []]
                  (Just $ ExpressionSend (OperandIdentifier "x") 
                    [] 
                    [BinaryMessage "+" (OperandIdentifier "y"), BinaryMessage "/" (OperandLiteral (LiteralNumber 2))]
                    []
                  )]
              )
            )
          [] [] []))

    it "does not parse statement with malformed binop" $ do
      parseStatement "a + + b" `shouldSatisfy` isLeft

    it "does not parse statement with malformed keyword message" $ do
      parseStatement "a b: c: d" `shouldSatisfy` isLeft

main = do
  hspec lexerSpec
  hspec parserSpec