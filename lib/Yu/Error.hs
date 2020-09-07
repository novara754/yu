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
parseErrorErrata :: ParseError [Located Tok] () CustomError CustomLabel -> Errata
parseErrorErrata (ParseError _ _ ei) =
    -- Because our parser has no custom state i.e. we chose not to keep track of source positions, not all errors can
    -- be printed correctly. However, we purposely constructed our parser in a way that those errors cannot ever occur,
    -- and that the errors that do occur will provide us with the correct source positions via our 'Span' type.
    case ei of
        ErrorItemHints (Just (HintToken (Located (Span fp _ (l1, c1) (l2, c2)) t))) hs -> errataSimple
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
        -- The below are things that can never happen due to the construction of our parser:
        -- No unexpected chunks, because we only ever parse one token at a time.
        -- No unexpected labels, because we don't produce unexpected labels.
        -- No unexpected end of input, because we have a special EOF token.
        -- No 'Nothing' for the unexpected item, since we only ever match on a token which will add an unexpected item.
        -- No errors from 'fail', because that should not happen outside of debugging.
        ErrorItemHints (Just (HintChunk _)) _ -> error "parseErrorErrata: unexpected chunk"
        ErrorItemHints (Just (HintLabel _)) _ -> error "parseErrorErrata: unexpected label"
        ErrorItemHints (Just HintEnd) _       -> error "parseErrorErrata: unexpected end of input"
        ErrorItemHints Nothing _              -> error "parseErrorErrata: no unexpected item"
        ErrorItemFail m                       -> error $ "parseErrorErrata, fail: " <> m
    where
        makeMessage :: Tok -> Hints [Located Tok] CustomLabel -> T.Text
        makeMessage t hs = "unexpected " <> prettyTok t <> "\nexpected " <> showHints (nub $ sort hs)

        showHints :: Hints [Located Tok] CustomLabel -> T.Text
        showHints [] = "nothing"
        showHints [x] = showHint x
        showHints [x, y] = showHint x <> " or " <> showHint y
        showHints xs = T.intercalate ", " (map showHint (init xs)) <> ", or " <> showHint (last xs)

        showHint :: Hint [Located Tok] CustomLabel -> T.Text
        showHint h = case h of
            HintLabel (LabelTok t)     -> prettyTok t
            HintLabel LabelModule      -> "a module"
            HintLabel LabelDeclaration -> "a declaration"
            HintLabel LabelStatement   -> "a statement"
            HintLabel LabelExpression  -> "an expression"
            HintLabel LabelIdent       -> "an identifier"
            HintLabel LabelOpIdent       -> "an operator"
            HintLabel LabelLiteral     -> "an integer"
            HintLabel LabelInt         -> "an integer"

            -- The below are things that can never happen due to the construction of our parser:
            -- No expected tokens or chunks, because we only use our custom labels. See 'Lexer.LabelTok' for why.
            -- No expected end of input, because we have a custom EOF token instead.
            HintToken _ -> error "showHint: expected token"
            HintChunk _ -> error "showHint: expected chunk"
            HintEnd     -> error "showHint: expected end of input"

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
