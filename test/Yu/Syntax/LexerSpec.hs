module Yu.Syntax.LexerSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import           Data.Void
import qualified Data.Text as T
import           Data.Text.Arbitrary()
import           Hectoparsec
import           Yu.Syntax.Lexer
import           Yu.Syntax.Span

-- | Helper function to lex text and remove location data to leave
--   only the actual tokens.
doLex :: T.Text -> Either (ParseError T.Text Void Void) [Tok]
doLex src = map lValue <$> evalParser pLexer "spec" src

spec :: Spec
spec = do
  describe "isIdent" $ do
    it "returns True for any Ident token" $ do
      isIdent (Ident "x")    `shouldBe` True
      isIdent (Ident "foo")  `shouldBe` True
      isIdent (Ident "quax") `shouldBe` True
      isIdent (Ident "test") `shouldBe` True

    it "returns False for anything that's not an Ident token" $ do
      or (map isIdent
        [ OpIdent "+++"
        , Keyword "return"
        , IntLiteral 123
        , LParen
        , RParen
        , LBrace
        , RBrace
        , Colon
        , Semicolon
        , Comma
        , Eq
        , Eof
        ]) `shouldBe` False

  describe "isOpIdent" $ do
    it "returns True for any OpIdent token" $ do
      isOpIdent (OpIdent "+")   `shouldBe` True
      isOpIdent (OpIdent "/")   `shouldBe` True
      isOpIdent (OpIdent "&&&") `shouldBe` True
      isOpIdent (OpIdent "<$>") `shouldBe` True

    it "returns False for anything that's not an OpIdent token" $ do
      or (map isOpIdent
        [ Ident "foo"
        , Keyword "return"
        , IntLiteral 123
        , LParen
        , RParen
        , LBrace
        , RBrace
        , Colon
        , Semicolon
        , Comma
        , Eq
        , Eof
        ]) `shouldBe` False

  describe "isInt" $ do
    it "returns True for any IntLiteral token" $ do
      isInt (IntLiteral 123)     `shouldBe` True
      isInt (IntLiteral 0)       `shouldBe` True
      isInt (IntLiteral 1512515) `shouldBe` True
      isInt (IntLiteral 24124)   `shouldBe` True

    it "returns False for anything that's not an IntLiteral token" $ do
      or (map isInt
        [ Ident "foo"
        , OpIdent "+++"
        , Keyword "return"
        , LParen
        , RParen
        , LBrace
        , RBrace
        , Colon
        , Semicolon
        , Comma
        , Eq
        , Eof
        ]) `shouldBe` False

  describe "pLexer" $ do
    it "lexes basic tokens" $ do
      let lexed = map doLex
            [ "return"
            , "fun"
            , "function"
            , "foo"
            , "x"
            , "bar"
            , "+"
            , "/"
            , "123 3335555a"
            , "(){}"
            , ":;,="
            ]
      let expected = map Right
            [ [Keyword "return", Eof]
            , [Keyword "fun", Eof]
            , [Ident "function", Eof]
            , [Ident "foo", Eof]
            , [Ident "x", Eof]
            , [Ident "bar", Eof]
            , [OpIdent "+", Eof]
            , [OpIdent "/", Eof]
            , [IntLiteral 123, IntLiteral 3335555, Ident "a", Eof]
            , [LParen, RParen, LBrace, RBrace, Eof]
            , [Colon, Semicolon, Comma, Eq, Eof]
            ]
      lexed `shouldBe` expected

    it "skips whitespace, line and block comments" $ do
      let lexed = doLex "let         x = /* 123 */ 321; // hello"
      let expected = Right
            [ Keyword "let"
            , Ident "x"
            , Eq
            , IntLiteral 321
            , Semicolon
            , Eof
            ]
      lexed `shouldBe` expected

    it "always ends the token stream with an Eof" . property $
      \src -> (last <$> doLex src) == Right Eof
