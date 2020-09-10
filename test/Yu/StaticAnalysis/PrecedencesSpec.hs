module Yu.StaticAnalysis.PrecedencesSpec (spec) where

import Test.Hspec

import Control.Monad.Writer.Strict
import Yu.Syntax.Span
import Yu.Syntax.Parser

import Yu.StaticAnalysis.Precedences
import Yu.StaticAnalysis.Precedences.Internal

-- | Empty span.
eSpan :: Span
eSpan = (Span "" (0, 0) (0, 0) (0, 0))

-- | Helper function to wrap an item in an empty location, a span with 0 size.
eLoc :: a -> Located a
eLoc = Located eSpan

spec :: Spec
spec = do
  describe "applyPrecedences" $ do
    it "doesnt change a module without binary operations" $ do
      let ast = Module
            [ ModuleDecl eSpan (eLoc "main")
            , FunctionDecl
                eSpan
                (eLoc "main")
                []
                Nothing
                [ VarDecl
                    eSpan
                    (eLoc "a")
                    (Just $ eLoc "int")
                    (Literal eSpan 5)
                , VarDecl
                    eSpan
                    (eLoc "b")
                    Nothing
                    (VarRef eSpan $ eLoc "a")
                , ExprStmt eSpan $ FuncCall
                    eSpan
                    (VarRef eSpan $ eLoc "print")
                    [VarRef eSpan $ eLoc "b"]
                ]
            ]
      runWriter (applyPrecedences ast) `shouldBe` (ast, [])

  describe "applyExpr" $ do
    it "leaves single binary operations alone" $ do
      let asts =
            [ BinOp eSpan (eLoc "+") (Literal eSpan  5) (Literal eSpan   3)
            , BinOp eSpan (eLoc "*") (Literal eSpan  1) (Literal eSpan   2)
            , BinOp eSpan (eLoc "/") (Literal eSpan  2) (Literal eSpan  10)
            , BinOp eSpan (eLoc "&") (Literal eSpan 98) (Literal eSpan 103)
            ]
      map applyExpr asts `shouldBe` asts

    it "reorders simple expressions with two binops" $ do
      let ast = BinOp
            eSpan
            (eLoc "*")
            (Literal eSpan 5)
            (BinOp
              eSpan
              (eLoc "+")
              (Literal eSpan 7)
              (Literal eSpan 3)
            )
      let expected = BinOp
            eSpan
            (eLoc "+")
            (BinOp
              eSpan
              (eLoc "*")
              (Literal eSpan 5)
              (Literal eSpan 7)
            )
            (Literal eSpan 3)
      applyExpr ast `shouldBe` expected

    it "reorders semi complex expressions with multiple binops" $ do
      let ast = BinOp
            eSpan
            (eLoc "/")
            (Literal eSpan 5)
            (BinOp
              eSpan
              (eLoc "+")
              (Literal eSpan 5)
              (BinOp
                eSpan
                (eLoc "*")
                (Literal eSpan 10)
                (Literal eSpan 5)
              )
            )
      let expected = BinOp
            eSpan
            (eLoc "+")
            (BinOp
              eSpan
              (eLoc "/")
                (Literal eSpan 5)
                (Literal eSpan 5)
              )
            (BinOp
              eSpan
              (eLoc "*")
              (Literal eSpan 10)
              (Literal eSpan 5)
            )
      applyExpr ast `shouldBe` expected

    it "reorders complex expressions with multiple binops" $ do
      let ast = BinOp
            eSpan
            (eLoc "+")
            (Literal eSpan 1)
            (BinOp
              eSpan
              (eLoc "*")
              (Literal eSpan 2)
              (BinOp
                eSpan
                (eLoc "/")
                (Literal eSpan 5)
                (BinOp
                  eSpan
                  (eLoc "+")
                  (Literal eSpan 5)
                  (BinOp
                    eSpan
                    (eLoc "*")
                    (Literal eSpan 10)
                    (Literal eSpan 5)
                  )
                )
              )
            )
      let expected = BinOp
            eSpan
            (eLoc "+")
            (Literal eSpan 1)
            (BinOp
              eSpan
              (eLoc "+")
              (BinOp
                eSpan
                (eLoc "*")
                (Literal eSpan 2)
                (BinOp
                  eSpan
                  (eLoc "/")
                  (Literal eSpan 5)
                  (Literal eSpan 5)
                )
              )
              (BinOp
                eSpan
                (eLoc "*")
                (Literal eSpan 10)
                (Literal eSpan 5)
              )
            )
      applyExpr ast `shouldBe` expected
