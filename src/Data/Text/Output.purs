module Data.Text.Output where

import Prelude

import Color as Color
import Data.String as String

import Type.Proxy (Proxy)

import Data.Text.Format (Tag(..))


data OutputKind


foreign import data Blessed :: OutputKind
-- foreign import data OneLine :: OutputKind
-- foreign import data PlainText :: OutputKind
-- foreign import data Markdown :: OutputKind
foreign import data Html :: OutputKind


data Options
    = OneLiner
    | IndentWithSpaces Int
    | IndentWithTabs


data Support
    = Full
    | Partly -- renders improperly but tries to apply formatting
    | Text -- just renders plain text it finds inside
    | None -- not even renders


class Renderer (x :: OutputKind) where
    supported :: Proxy x -> Tag -> Boolean
    renderTo :: Proxy x -> Tag -> String


render :: Tag -> String
render = case _ of
    Plain str -> str
    FgC color tagged -> wrap (Color.toHexString color <> "-fg") $  tagged
    Fg color tagged -> wrap (color <> "-fg") tagged
    BgC color tagged -> wrap (Color.toHexString color <> "-bg") tagged
    Bg color tagged -> wrap (color <> "-bg") tagged
    Align align tagged -> wrap (show align) tagged
    Format format tagged -> wrap (show format) tagged
    Split taggedA taggedB -> render taggedA <> "{|}" <> render taggedB
    Pair taggedA taggedB -> render taggedA <> render taggedB
    Nest _ taggedArr -> String.joinWith "" $ render <$> taggedArr
    Newline -> "\n"
    _ -> "" -- FIXME
    where
        wrap tag tagged = "{" <> tag <> "}" <> render tagged <> "{/" <> tag <> "}"