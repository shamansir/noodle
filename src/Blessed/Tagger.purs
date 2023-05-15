module Blessed.Tagger where

import Prelude

import Color (Color)
import Color as Color

import Data.Array (singleton)
import Data.String (joinWith) as String


data Align
    = Left
    | Right
    | Center


data Format
    = Bold
    | Underline
    | Blink
    | Inverse
    | Invisible


data Tag
    = Plain String
    | FgC Color Tagged
    | Fg String Tagged
    | BgC Color Tagged
    | Bg String Tagged
    | Align Align Tagged
    | Format Format Tagged
    | Split Tagged Tagged



instance Show Align where
    show Left = "left"
    show Right = "right"
    show Center = "center"


instance Show Format where
    show Bold = "bold"
    show Underline = "underline"
    show Blink = "blink"
    show Inverse = "inverse"
    show Invisible = "invisible"



plains :: String -> Tagged
plains = Tagged <<< singleton <<< plain


plain :: String -> Tag
plain = Plain


lefts :: Tagged -> Tagged
lefts = Tagged <<< singleton <<< left


left :: Tagged -> Tag
left = Align Left


rights :: Tagged -> Tagged
rights = Tagged <<< singleton <<< right


right :: Tagged -> Tag
right = Align Right


centers :: Tagged -> Tagged
centers = Tagged <<< singleton <<< right


center :: Tagged -> Tag
center = Align Center


-- TODO: do syntax?


newtype Tagged = Tagged (Array Tag)

instance Show Tagged where
    show = render



render :: Tagged -> String
render (Tagged arr) = String.joinWith "" $ tagToString <$> arr


tagToString :: Tag -> String
tagToString = case _ of
    Plain str -> str
    FgC color tagged -> wrap (Color.toHexString color <> "-fg") tagged
    Fg color tagged -> wrap (color <> "-fg") tagged
    BgC color tagged -> wrap (Color.toHexString color <> "-bg") tagged
    Bg color tagged -> wrap (color <> "-bg") tagged
    Align align tagged -> wrap (show align) tagged
    Format format tagged -> wrap (show format) tagged
    Split taggedA taggedB -> render taggedA <> "{|}" <> render taggedB
    where
        wrap tag tagged = "{" <> tag <> "}" <> render tagged <> "{/" <> tag <> "}"