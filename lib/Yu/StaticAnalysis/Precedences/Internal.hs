{-|
Module      : Yu.StaticAnalysis.Precedences.Internal
Description : Operator precedence transformations.

Apply operator precedences to a freshly parsed Yu module.

This module is internal and may break between non-major versions.
-}

module Yu.StaticAnalysis.Precedences.Internal
  ( applyDecl
  , applyStmt
  , applyExpr
  ) where

import qualified Data.Map as M
import qualified Data.Text as T

import           Yu.Syntax.Lexer
import           Yu.Syntax.Parser

-- | Get precedence for operator
prec :: T.Text -> Int
prec op = M.findWithDefault minBound op precs
  where
    precs = M.fromList
      [ ("+", 0)
      , ("-", 0)
      , ("*", 1)
      , ("/", 1)
      , ("%", 1)
      ]

-- | Apply operator precedences to a declaration.
applyDecl :: Decl -> Decl
applyDecl (ModuleDecl n)                = ModuleDecl n
applyDecl (FunctionDecl n ps ret stmts) = FunctionDecl n ps ret $ map (fmap applyStmt) stmts

-- | Apply operator precedences to a statement.
applyStmt :: Stmt -> Stmt
applyStmt (VarDecl n ty v) = VarDecl n ty $ (fmap applyExpr) v
applyStmt (ExprStmt v)     = ExprStmt $ fmap applyExpr v
applyStmt (Return v)       = Return $ fmap applyExpr v

-- | Apply operator precedences to an expression.
applyExpr :: Expr -> Expr
applyExpr   (Grouped e)        = Grouped $ fmap applyExpr e
applyExpr   (ExprIntLiteral v) = ExprIntLiteral v
applyExpr   (VarRef n)         = VarRef n
applyExpr   (FuncCall f ps)    = FuncCall (fmap applyExpr f) $ map (fmap applyExpr) ps
applyExpr b@(BinOp op1 l1 r1)  =
  let r1' = fmap applyExpr r1
  in case r1' of
    Located s (BinOp op2 l2 r2) ->
      let p1 = prec $ lValue op1
          p2 = prec $ lValue op2
      in if p1 <= p2
        then BinOp op1 l1 r1'
        else BinOp op2 (Located s $ BinOp op1 l1 l2) (fmap applyExpr r2)
    _ -> b
