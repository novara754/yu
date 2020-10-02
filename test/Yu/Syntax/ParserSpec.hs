module Yu.Syntax.ParserSpec (spec) where

import Test.Hspec

import Data.Kind
import Hectoparsec
import Yu.Syntax.Span
import Yu.Syntax.Lexer
import Yu.Syntax.Parser
import Yu.Syntax.Error

-- | Helper class to strip decorations from a parse tree.
class StripDec (n :: Phase -> Type) where
  strip :: n (d :: Phase) -> n 'NoDec

instance StripDec Module where
  strip (Module ds) = Module $ map strip ds

instance StripDec Decl where
  strip (ModuleDecl _ n)         = ModuleDecl () n
  strip (FunctionDecl _ n p r b) = FunctionDecl () n p r $ map strip b

instance StripDec Stmt where
  strip (VarDecl _ n t v) = VarDecl () n t $ strip v
  strip (ExprStmt _ e)    = ExprStmt () $ strip e
  strip (Return _ e)      = Return () $ strip e

instance StripDec Expr where
  strip (Literal _ v)    = Literal () v
  strip (VarRef _ n)     = VarRef () n
  strip (BinOp _ o l r)  = BinOp () o (strip l) (strip r)
  strip (FuncCall _ f p) = FuncCall () (strip f) (map strip p)
  strip (Grouped _ e)    = Grouped () (strip e)

-- | Helper function to wrap an item in an empty location, a span with 0 size.
eLoc :: a -> Located a
eLoc x = Located (Span "" (0,0) (0,0)) x

-- | Helper function to parse a tokenstream text and remove location data to leave
--   only the actual tokens. Automatically gives each token in the input
--   an empty location.
doParse :: [Tok] -> Either (ParseError TokStream CustomParserError CustomLabel) (Module 'NoDec)
doParse = fmap strip . evalParser pModule "spec" . TokStream . map eLoc

spec :: Spec
spec = do
  describe "pModule" $ do
    it "parses an empty module" $ do
      let ast = doParse [ Keyword "module", Ident "main", Semicolon, Eof]
      let expected = Right $ Module [ModuleDecl () (eLoc "main")]
      ast `shouldBe` expected

    it "parses a module with one empty function" $ do
      -- let ast = doParse $ moduleDecl "main" <> functionDecl "main" [] Nothing [] <> [Eof]
      let ast = doParse
            [ Keyword "module", Ident "main", Semicolon
            , Keyword "fun", Ident "main", LParen, RParen, LBrace, RBrace
            , Eof
            ]
      let expected = Right $ Module
            [ ModuleDecl () (eLoc "main")
            , FunctionDecl () (eLoc "main") [] Nothing []
            ]
      ast `shouldBe` expected

    it "parses a module with one function with params and return" $ do
      let ast = doParse
            [ Keyword "module", Ident "math", Semicolon
            , Keyword "fun", Ident "add", LParen, Ident "x", Colon, Ident "int", Comma
            , Ident "y", Colon, Ident "int", Comma, Ident "z", Colon, Ident "int", RParen
            , Colon, Ident "int", LBrace
            , Keyword "return", Ident "x", OpIdent "+", Ident "y", OpIdent "+", Ident "z", Semicolon
            , RBrace
            , Eof
            ]
      let expected = Right $ Module
            [ ModuleDecl () (eLoc "math")
            , FunctionDecl
                ()
                (eLoc "add")
                [ Param (eLoc "x") (eLoc "int")
                , Param (eLoc "y") (eLoc "int")
                , Param (eLoc "z") (eLoc "int")
                ]
                (Just $ eLoc "int")
                [ Return () $ BinOp
                    ()
                    (eLoc "+")
                    (VarRef () $ eLoc "x")
                    (BinOp
                      ()
                      (eLoc "+")
                      (VarRef () $ eLoc "y")
                      (VarRef () $ eLoc "z")
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
            [ ModuleDecl () (eLoc "main")
            , FunctionDecl
                ()
                (eLoc "main")
                []
                Nothing
                [ VarDecl
                    ()
                    (eLoc "a")
                    (Just $ eLoc "int")
                    (Literal () 5)
                , VarDecl
                    ()
                    (eLoc "b")
                    Nothing
                    (VarRef () $ eLoc "a")
                , ExprStmt () $ FuncCall
                    ()
                    (VarRef () $ eLoc "print")
                    [VarRef () $ eLoc "b"]
                ]
            ]
      ast `shouldBe` expected
