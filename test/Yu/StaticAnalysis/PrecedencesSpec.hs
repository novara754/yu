module Yu.StaticAnalysis.PrecedencesSpec (spec) where

import Test.Hspec

import Control.Monad.Writer
import Yu.Syntax.Span
import Yu.Syntax.Parser

import Yu.StaticAnalysis.Precedences
import Yu.StaticAnalysis.Precedences.Internal

-- | Empty span.
eSpan :: Span
eSpan = (Span "" (0, 0) (0, 0))

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
                    (Literal eSpan (LiteralInt 5))
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
            [ BinOp eSpan (eLoc "+") (Literal eSpan (LiteralInt  5)) (Literal eSpan (LiteralInt   3))
            , BinOp eSpan (eLoc "*") (Literal eSpan (LiteralInt  1)) (Literal eSpan (LiteralInt   2))
            , BinOp eSpan (eLoc "/") (Literal eSpan (LiteralInt  2)) (Literal eSpan (LiteralInt  10))
            , BinOp eSpan (eLoc "&") (Literal eSpan (LiteralInt 98)) (Literal eSpan (LiteralInt 103))
            ]
      map applyExpr asts `shouldBe` asts

    it "reorders simple expressions with two binops" $ do
      let ast = BinOp
            eSpan
            (eLoc "*")
            (Literal eSpan (LiteralInt 5))
            (BinOp
              eSpan
              (eLoc "+")
              (Literal eSpan (LiteralInt 7))
              (Literal eSpan (LiteralInt 3))
            )
      let expected = BinOp
            eSpan
            (eLoc "+")
            (BinOp
              eSpan
              (eLoc "*")
              (Literal eSpan (LiteralInt 5))
              (Literal eSpan (LiteralInt 7))
            )
            (Literal eSpan (LiteralInt 3))
      applyExpr ast `shouldBe` expected

    it "reorders semi complex expressions with multiple binops" $ do
      let ast = BinOp
            eSpan
            (eLoc "/")
            (Literal eSpan (LiteralInt 5))
            (BinOp
              eSpan
              (eLoc "+")
              (Literal eSpan (LiteralInt 5))
              (BinOp
                eSpan
                (eLoc "*")
                (Literal eSpan (LiteralInt 10))
                (Literal eSpan (LiteralInt 5))
              )
            )
      let expected = BinOp
            eSpan
            (eLoc "+")
            (BinOp
              eSpan
              (eLoc "/")
                (Literal eSpan (LiteralInt 5))
                (Literal eSpan (LiteralInt 5))
              )
            (BinOp
              eSpan
              (eLoc "*")
              (Literal eSpan (LiteralInt 10))
              (Literal eSpan (LiteralInt 5))
            )
      applyExpr ast `shouldBe` expected

    it "reorders complex expressions with multiple binops" $ do
      let ast = BinOp
            eSpan
            (eLoc "+")
            (Literal eSpan (LiteralInt 1))
            (BinOp
              eSpan
              (eLoc "*")
              (Literal eSpan (LiteralInt 2))
              (BinOp
                eSpan
                (eLoc "/")
                (Literal eSpan (LiteralInt 5))
                (BinOp
                  eSpan
                  (eLoc "+")
                  (Literal eSpan (LiteralInt 5))
                  (BinOp
                    eSpan
                    (eLoc "*")
                    (Literal eSpan (LiteralInt 10))
                    (Literal eSpan (LiteralInt 5))
                  )
                )
              )
            )
      let expected = BinOp
            eSpan
            (eLoc "+")
            (Literal eSpan (LiteralInt 1))
            (BinOp
              eSpan
              (eLoc "+")
              (BinOp
                eSpan
                (eLoc "*")
                (Literal eSpan (LiteralInt 2))
                (BinOp
                  eSpan
                  (eLoc "/")
                  (Literal eSpan (LiteralInt 5))
                  (Literal eSpan (LiteralInt 5))
                )
              )
              (BinOp
                eSpan
                (eLoc "*")
                (Literal eSpan (LiteralInt 10))
                (Literal eSpan (LiteralInt 5))
              )
            )
      applyExpr ast `shouldBe` expected

    it "accurately updates spans on reordered binops" $ do
      let ast = BinOp
            (Span "<test>" (1,1) (1,6))
            (Located (Span "<test>" (1,2) (1,3)) "*")
            (Literal (Span "<test>" (1,1) (1,2)) (LiteralInt 5))
            (BinOp
              (Span "<test>"(1,3) (1,5))
              (Located (Span "<test>" (1,4) (1,5)) "+")
              (Literal (Span "<test>" (1,3) (1,4)) (LiteralInt 7))
              (Literal (Span "<test>" (1,5) (1,6)) (LiteralInt 3))
            )
      let expected = BinOp
            (Span "<test>" (1,1) (1,6))
            (Located (Span "<test>" (1,4) (1,5)) "+")
            (BinOp
              (Span "<test>" (1,1) (1,4))
              (Located (Span "<test>" (1,2) (1,3)) "*")
              (Literal (Span "<test>" (1,1) (1,2)) (LiteralInt 5))
              (Literal (Span "<test>" (1,3) (1,4)) (LiteralInt 7))
            )
            (Literal (Span "<test>" (1,5) (1,6)) (LiteralInt 3))
      applyExpr ast `shouldBe` expected
