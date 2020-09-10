module Yu.StaticAnalysis.PrecedencesSpec (spec) where

import Test.Hspec

import Control.Monad.Writer.Strict
import Yu.Syntax.Lexer
import Yu.Syntax.Parser

import Yu.StaticAnalysis.Precedences
import Yu.StaticAnalysis.Precedences.Internal

-- | Helper function to wrap an item in an empty location, a span with 0 size.
eLoc :: a -> Located a
eLoc x = Located (Span "" (0, 0) (0, 0) (0, 0)) x

spec :: Spec
spec = do
  describe "applyPrecedences" $ do
    it "doesnt change a module without binary operations" $ do
      let ast = Module
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
      runWriter (applyPrecedences ast) `shouldBe` (ast, [])

  describe "applyExpr" $ do
    it "leaves single binary operations alone" $ do
      let asts =
            [ BinOp (eLoc "+") (eLoc $ ExprIntLiteral  5) (eLoc $ ExprIntLiteral   3)
            , BinOp (eLoc "*") (eLoc $ ExprIntLiteral  1) (eLoc $ ExprIntLiteral   2)
            , BinOp (eLoc "/") (eLoc $ ExprIntLiteral  2) (eLoc $ ExprIntLiteral  10)
            , BinOp (eLoc "&") (eLoc $ ExprIntLiteral 98) (eLoc $ ExprIntLiteral 103)
            ]
      map applyExpr asts `shouldBe` asts

    it "reorders simple expressions with two binops" $ do
      let ast = BinOp
            (eLoc "*")
            (eLoc $ ExprIntLiteral 5)
            (eLoc $ BinOp
              (eLoc "+")
              (eLoc $ ExprIntLiteral 7)
              (eLoc $ ExprIntLiteral 3)
            )
      let expected = BinOp
            (eLoc "+")
            (eLoc $ BinOp
              (eLoc "*")
              (eLoc $ ExprIntLiteral 5)
              (eLoc $ ExprIntLiteral 7)
            )
            (eLoc $ ExprIntLiteral 3)
      applyExpr ast `shouldBe` expected

    it "reorders semi complex expressions with multiple binops" $ do
      let ast = BinOp
            (eLoc "/")
            (eLoc $ ExprIntLiteral 5)
            (eLoc $ BinOp
              (eLoc "+")
              (eLoc $ ExprIntLiteral 5)
              (eLoc $ BinOp
                (eLoc "*")
                (eLoc $ ExprIntLiteral 10)
                (eLoc $ ExprIntLiteral 5)
              )
            )
      let expected = BinOp
            (eLoc "+")
            (eLoc $ BinOp
              (eLoc "/")
                (eLoc $ ExprIntLiteral 5)
                (eLoc $ ExprIntLiteral 5)
              )
            (eLoc $ BinOp
              (eLoc "*")
              (eLoc $ ExprIntLiteral 10)
              (eLoc $ ExprIntLiteral 5)
            )
      applyExpr ast `shouldBe` expected

    it "reorders complex expressions with multiple binops" $ do
      let ast = BinOp
            (eLoc $ "+")
            (eLoc $ ExprIntLiteral 1)
            (eLoc $ BinOp
              (eLoc "*")
              (eLoc $ ExprIntLiteral 2)
              (eLoc $ BinOp
                (eLoc "/")
                (eLoc $ ExprIntLiteral 5)
                (eLoc $ BinOp
                  (eLoc "+")
                  (eLoc $ ExprIntLiteral 5)
                  (eLoc $ BinOp
                    (eLoc "*")
                    (eLoc $ ExprIntLiteral 10)
                    (eLoc $ ExprIntLiteral 5)
                  )
                )
              )
            )
      let expected = BinOp
            (eLoc "+")
            (eLoc $ ExprIntLiteral 1)
            (eLoc $ BinOp
              (eLoc "+")
              (eLoc $ BinOp
                (eLoc "*")
                (eLoc $ ExprIntLiteral 2)
                (eLoc $ BinOp
                  (eLoc "/")
                  (eLoc $ ExprIntLiteral 5)
                  (eLoc $ ExprIntLiteral 5)
                )
              )
              (eLoc $ BinOp
                (eLoc "*")
                (eLoc $ ExprIntLiteral 10)
                (eLoc $ ExprIntLiteral 5)
              )
            )
      applyExpr ast `shouldBe` expected
