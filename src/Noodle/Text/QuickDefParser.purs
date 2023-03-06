module Noodle.Text.QuickDefParser where

import Prelude

import Data.String.CodeUnits as String
import Data.Array as Array
import Data.Maybe (Maybe(..))

import Text.Parsing.Parser (ParserT) as P
import Text.Parsing.Parser.String (char, string)  as P
import Text.Parsing.Parser.Token (alphaNum, space) as P
import Text.Parsing.Parser.Combinators (between, choice, option, optionMaybe, sepBy) as P

import Noodle.Text.QuickDef as QD


fnParser :: forall (m :: Type -> Type). Functor m => Monad m => P.ParserT String m QD.FN
fnParser = do
  family <- validToken
  _ <- Array.some P.space
  _ <- P.char ':'
  _ <- Array.some P.space
  fnName <- validToken
  _ <- Array.some P.space
  _ <- P.string "::"
  _ <- Array.many P.space
  args <- P.option [] argumentsP
  _ <- Array.many P.space
  _ <- P.string "=>"
  _ <- Array.some P.space
  returnVal <- validToken
  pure $ QD.qfn' family fnName args returnVal

  where
    validToken = String.fromCharArray <$> Array.some P.alphaNum
    validDefaultValue = String.fromCharArray <$> Array.some (P.choice [ P.alphaNum, P.char '.' ])
    validArgument = do
      P.choice
        [ P.char '?' *> pure Nothing
        , Just <$> do
            name <- validToken
            maybeType <- P.optionMaybe $ P.char ':' *> validToken
            _ <- Array.many P.space
            maybeDefault <- P.optionMaybe
                              $ P.between
                                (P.char '{')
                                (P.char '}')
                                validDefaultValue
            pure { name, type : maybeType, default : maybeDefault }
        ]
    arrowSep = do
      _ <- Array.many P.space
      _ <- P.choice [ P.string "->", String.singleton <$> P.char '→' ]
      _ <- Array.many P.space
      pure unit
    argumentsP =
      P.between
        (P.char '<')
        (P.char '>')
        $ Array.fromFoldable <$> P.sepBy validArgument arrowSep


fnListParser :: forall (m :: Type -> Type). Functor m => Monad m => P.ParserT String m (Array QD.FN)
fnListParser = Array.many $ fnParser >>= \fn -> P.char '\n' *> pure fn