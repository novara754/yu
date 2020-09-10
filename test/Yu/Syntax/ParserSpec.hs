module Yu.Syntax.ParserSpec (spec) where

import Test.Hspec

import           Hectoparsec.Parser
import           Yu.Syntax.Lexer
import           Yu.Syntax.Parser
import           Yu.Error

-- | Helper function to wrap an item in an empty location, a span with 0 size.
eLoc :: a -> Located a
eLoc x = Located (Span "" (0, 0) (0, 0) (0, 0)) x

-- | Helper function to parse a tokenstream text and remove location data to leave
--   only the actual tokens. Automatically gives each token in the input
--   an empty location.
doParse :: [Tok] -> Either (ParseError [Located Tok] CustomError CustomLabel) Module
doParse = evalParser pModule . map eLoc

spec :: Spec
spec = do
  describe "pModule" $ do
    it "parses an empty module" $ do
      let ast = doParse [ Keyword "module", Ident "main", Semicolon, Eof]
      let expected = Right $ Module [eLoc $ ModuleDecl (eLoc "main")]
      ast `shouldBe` expected

    it "parses a module with one empty function" $ do
      -- let ast = doParse $ moduleDecl "main" <> functionDecl "main" [] Nothing [] <> [Eof]
      let ast = doParse
            [ Keyword "module", Ident "main", Semicolon
            , Keyword "fun", Ident "main", LParen, RParen, LBrace, RBrace
            , Eof
            ]
      let expected = Right $ Module
            [ eLoc $ ModuleDecl (eLoc "main")
            , eLoc $ FunctionDecl (eLoc "main") [] Nothing []
            ]
      ast `shouldBe` expected

    it "parses a module with one function with params and return" $ do
      let ast = doParse
            [ Keyword "module", Ident "math", Semicolon
            , Keyword "fun", Ident "add", LParen, Ident "x", Colon, Ident "int", Comma
            , Ident "y", Colon, Ident "int", Comma , Ident "z", Colon, Ident "int", RParen
            , Colon, Ident "int", LBrace
            , Keyword "return", Ident "x", OpIdent "+", Ident "y", OpIdent "+", Ident "z", Semicolon
            , RBrace
            , Eof
            ]
      let expected = Right $ Module
            [ eLoc $ ModuleDecl (eLoc "math")
            , eLoc $ FunctionDecl
                (eLoc "add")
                [ Param (eLoc "x") (eLoc "int")
                , Param (eLoc "y") (eLoc "int")
                , Param (eLoc "z") (eLoc "int")
                ]
                (Just $ eLoc "int")
                [ eLoc . Return . eLoc $ BinOp
                    (eLoc "+")
                    (eLoc . VarRef $ eLoc "x")
                    (eLoc $ BinOp
                      (eLoc "+")
                      (eLoc . VarRef $ eLoc "y")
                      (eLoc . VarRef $ eLoc "z")
                    )
                ]
            ]
      ast `shouldBe` expected

    it "parses a module with one more complex function" $ do
      let ast = doParse
            [ Keyword "module", Ident "main", Semicolon
            , Keyword "fun", Ident "main", LParen, RParen, LBrace
            , Keyword "let", Ident "a", Colon, Ident "int", Eq, IntLiteral 5, Semicolon
            , Keyword "let", Ident "b", Eq, Ident "a", Semicolon
            , Ident "print", LParen, Ident "b", RParen, Semicolon
            , RBrace
            , Eof
            ]
      let expected = Right $ Module
            [ eLoc $ ModuleDecl (eLoc "main")
            , eLoc $ FunctionDecl
                (eLoc "main")
                []
                Nothing
                [ eLoc $ VarDecl
                    (eLoc "a")
                    (Just $ eLoc "int")
                    (eLoc $ ExprIntLiteral 5)
                , eLoc $ VarDecl
                    (eLoc "b")
                    Nothing
                    (eLoc . VarRef $ eLoc "a")
                , eLoc . ExprStmt . eLoc $ FuncCall
                    (eLoc . VarRef $ eLoc "print")
                    [eLoc . VarRef $ eLoc "b"]
                ]
            ]
      ast `shouldBe` expected
