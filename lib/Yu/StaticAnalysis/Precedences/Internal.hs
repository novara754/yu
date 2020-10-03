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

import           Prelude hiding (span)
import qualified Data.Map as M
import qualified Data.Text as T

import           Yu.Syntax.Parser
import           Yu.Syntax.Span

-- | Get precedence for operator
prec :: T.Text -> Int
prec op = M.findWithDefault minBound op precs
  where
    precs = M.fromList
      [ ("==", 0)
      , ("!=", 0)
      , ("<", 1)
      , (">", 1)
      , ("<=", 1)
      , (">=", 1)
      , ("+", 3)
      , ("-", 3)
      , ("*", 4)
      , ("/", 4)
      , ("%", 4)
      ]

-- | Apply operator precedences to a declaration.
applyDecl :: Decl 'Parse -> Decl 'Parse
applyDecl (ModuleDecl s n)                = ModuleDecl s n
applyDecl (FunctionDecl s n ps ret stmts) = FunctionDecl s n ps ret $ map applyStmt stmts

-- | Apply operator precedences to a statement.
applyStmt :: Stmt 'Parse -> Stmt 'Parse
applyStmt (VarDecl s n ty v) = VarDecl s n ty $ applyExpr v
applyStmt (ExprStmt s v)     = ExprStmt s $ applyExpr v
applyStmt (Return s v)       = Return s $ applyExpr v

-- | Apply operator precedences to an expression.
applyExpr :: Expr 'Parse -> Expr 'Parse
applyExpr   (Grouped s e)      = Grouped s $ applyExpr e
applyExpr   (Literal s v)      = Literal s v
applyExpr   (VarRef s n)       = VarRef s n
applyExpr   (FuncCall s f ps)  = FuncCall s (applyExpr f) $ map applyExpr ps
applyExpr b@(BinOp s1 op1 l1 r1)  =
  let r1' = applyExpr r1
  in case r1' of
    BinOp _ op2 l2 r2 ->
      let p1 = prec $ lValue op1
          p2 = prec $ lValue op2
      in if p1 <= p2
        then BinOp s1 op1 l1 r1'
        else BinOp s1 op2 (BinOp (span l1 <> span l2) op1 l1 l2) $ applyExpr r2
    _ -> b
