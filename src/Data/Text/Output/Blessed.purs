module Data.Text.Output.Blessed where

import Prelude

import Color as Color

import Data.Either (Either(..))
import Data.String (joinWith) as String
import Data.Text.Format (Tag(..), Format(..))
import Data.Text.Output (OutputKind, class Renderer)


foreign import data Blessed :: OutputKind



render :: Tag -> String
render = case _ of
    Plain str -> str
    Align align tagged -> wrap (show align) tagged
    Format (Fg (Left colorStr)) tagged -> wrap (colorStr <> "-fg") tagged
    Format (Fg (Right color)) tagged -> wrap (Color.toHexString color <> "-fg") tagged
    Format (Bg (Left colorStr)) tagged -> wrap (colorStr <> "-bg") tagged
    Format (Bg (Right color)) tagged -> wrap (Color.toHexString color <> "-bg") tagged
    Format format tagged -> wrap (show format) tagged
    Split taggedA taggedB -> render taggedA <> "{|}" <> render taggedB
    Pair taggedA taggedB -> render taggedA <> render taggedB
    Nest _ taggedArr -> String.joinWith "" $ render <$> taggedArr
    Newline -> "\n"
    _ -> "" -- FIXME
    where
        wrap tag tagged = "{" <> tag <> "}" <> render tagged <> "{/" <> tag <> "}"