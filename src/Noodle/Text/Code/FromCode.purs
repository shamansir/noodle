module Noodle.Text.FromCode where

import Prelude

import Type.Proxy (Proxy)
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Bifunctor (lmap)

import Parsing (Parser, runParser, ParseError, parseErrorMessage)

import Noodle.Text.Code.Target (Target)


type Source = String


data SourceError
    = ParsingFailed ParseError
    | ConversionFailed { reason :: String }
    | UnknownError


class CanParse (target :: Target) a where
    parser :: Proxy target -> Parser Source a


class FromCode (target :: Target) opts a where
    fromCode :: Proxy target -> opts -> Source -> Either SourceError a


{-
instance CanParse target a => FromCode target opts a where
    fromCode :: Proxy target -> opts -> String -> Maybe a
    fromCode pTarget _ string = runParser string (parser pTarget :: Parser String a) # Either.hush
-}


fromParser :: forall target opts a. CanParse target a => Proxy target -> opts -> Source -> Either SourceError a
fromParser pTarget _ source = runParser source (parser pTarget :: Parser Source a) # lmap ParsingFailed


srcErrorToString :: SourceError -> String
srcErrorToString = case _ of
    ParsingFailed perr -> parseErrorMessage perr
    ConversionFailed { reason } -> reason
    UnknownError -> "Unknown"