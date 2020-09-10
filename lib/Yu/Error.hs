{-|
Module      : Yu.Error
Description : Error & label types and pretty printing.

This module contains types for Yu language errors and labels for language items.
Also contains functions to parse these errors for use with Errata.
-}
module Yu.Error
 ( CustomLabel(..)
 , CustomError(..)
 , parseErrorErrata
 ) where

import           Data.List (nub, sort)
import qualified Data.Text as T
import           Errata
import           Hectoparsec

import           Yu.Syntax.Lexer

-- | Custom labels for the parser.
data CustomLabel
  = LabelTok Tok
  | LabelModule
  | LabelDeclaration
  | LabelStatement
  | LabelExpression
  | LabelLiteral
  | LabelIdent
  | LabelOpIdent
  | LabelInt
  deriving (Show, Eq, Ord)

-- | Custom errors for the parser.
data CustomError
  = CustomError
  deriving (Show, Eq, Ord)

-- | Turn parser errors into Errata errors.
parseErrorErrata :: ParseError [Located Tok] CustomError CustomLabel -> Errata
parseErrorErrata (ParseError _ ei) =
    -- Because our parser has no custom state i.e. we chose not to keep track of source positions, not all errors can
    -- be printed correctly. However, we purposely constructed our parser in a way that those errors cannot ever occur,
    -- and that the errors that do occur will provide us with the correct source positions via our 'Span' type.
    case ei of
        ErrorItemHints (Located (Span fp _ (l1, c1) (l2, c2)) t) hs -> errataSimple
          (Just $ red "error: unexpected item")
          (blockMerged'
              fancyRedStyle
              fp
              (l1, c1, Nothing)
              (l2, c2 - 1, Nothing)
              Nothing
              (Just $ makeMessage t hs))
          Nothing

        ErrorItemCustom _ -> error "parseErrorErrata: no custom errors emitted yet"
        ErrorItemFail m   -> error $ "parseErrorErrata, fail: " <> m
    where
        makeMessage :: Tok -> [CustomLabel] -> T.Text
        makeMessage t hs = "unexpected " <> prettyTok t <> "\nexpected " <> showHints (nub $ sort hs)

        showHints :: [CustomLabel] -> T.Text
        showHints [] = "nothing"
        showHints [x] = showHint x
        showHints [x, y] = showHint x <> " or " <> showHint y
        showHints xs = T.intercalate ", " (map showHint (init xs)) <> ", or " <> showHint (last xs)

        showHint :: CustomLabel -> T.Text
        showHint h = case h of
            LabelTok t       -> prettyTok t
            LabelModule      -> "a module"
            LabelDeclaration -> "a declaration"
            LabelStatement   -> "a statement"
            LabelExpression  -> "an expression"
            LabelIdent       -> "an identifier"
            LabelOpIdent     -> "an operator"
            LabelLiteral     -> "an integer"
            LabelInt         -> "an integer"

        prettyTok :: Tok -> T.Text
        prettyTok t = case t of
            Ident x -> "identifier " <> x
            OpIdent x -> "operator `" <> x <> "`"
            Keyword x -> x
            IntLiteral n -> T.pack (show n)
            LBrace -> "`{`"
            RBrace -> "`}`"
            LParen -> "`(`"
            RParen -> "`)`"
            Semicolon -> "`;`"
            Colon -> "`:`"
            Comma -> "`,`"
            Eq -> "`=`"
            Eof -> "end of input"
            Unknown c -> "`" <> T.singleton c <> "`"

red :: T.Text -> T.Text
red t = "\x1b[31m" <> t <> "\x1b[0m"
