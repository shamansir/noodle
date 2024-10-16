module Noodle.Text.FromCode where

import Prelude

import Type.Proxy (Proxy)
import Data.Maybe (Maybe)
import Data.Either (hush) as Either

import Parsing (Parser, runParser)

import Noodle.Text.Code.Target (Target)


class CanParse (target :: Target) a where
    parser :: Proxy target -> Parser String a


class FromCode (target :: Target) opts a where
    fromCode :: Proxy target -> opts -> String -> Maybe a


{-
instance CanParse target a => FromCode target opts a where
    fromCode :: Proxy target -> opts -> String -> Maybe a
    fromCode pTarget _ string = runParser string (parser pTarget :: Parser String a) # Either.hush
-}


fromParser :: forall target opts a. CanParse target a => Proxy target -> opts -> String -> Maybe a
fromParser pTarget _ string = runParser string (parser pTarget :: Parser String a) # Either.hush
