module Blessed.Tagger where

import Prelude

import Color (Color)
import Color as Color

import Data.Array (singleton)
import Data.String (joinWith) as String

import Type.Proxy (Proxy)


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
    -- | Strikethrough
    -- | Monospaced
    -- | Header Level (Maybe Id)
    -- | Quote
    -- | Verbatim
    -- | Footnote FtnId
    -- | Code Language
    -- | Sub
    -- | Sup


newtype Indent = Indent Int


-- data ListKind
--     = Ordered TheWay
--     | Unordered


-- data TaskStatus
--     = Done
--     | Doing
--     | ProgressPercent Int
--     | ProgressOf Int Int
--     | AutoProgress


data Tag
    = Plain String
    | FgC Color Tag
    | Fg String Tag
    | BgC Color Tag
    | Bg String Tag
    | Align Align Tag
    | Format Format Tag
    | Split Tag Tag
    | Pair Tag Tag
    | Nest Indent (Array Tag)
    | Newline
    -- | Date Date
    -- | Header Level Tag
    -- | List Tag (Array Tag)
    -- | Table (Array (Tag /\ Array Tag)))
    -- | Link Tag Tag
    -- | LinkTo Tag FtnId
    -- | Definition Tag Tag
    -- | Hr
    -- | Image String
    -- | Property String String
    -- | Comment String
    -- | Macro String


-- TODO: binary operators for tags
-- TODO: empty tag



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


instance Semigroup Tag where
    append :: Tag -> Tag -> Tag
    append = Pair


s :: String -> Tag
s = plain


plain :: String -> Tag
plain = Plain


lefts :: String -> Tag
lefts = left <<< plain


left :: Tag -> Tag
left = Align Left


rights :: String -> Tag
rights = right <<< plain


right :: Tag -> Tag
right = Align Right


centers :: String -> Tag
centers = center <<< plain


center :: Tag -> Tag
center = Align Center


bolds :: String -> Tag
bolds = bold <<< plain


bold :: Tag -> Tag
bold = Format Bold


underlines :: String -> Tag
underlines = underline <<< plain


underline :: Tag -> Tag
underline = Format Underline


blinks :: String -> Tag
blinks = blink <<< plain


blink :: Tag -> Tag
blink = Format Blink


inverses :: String -> Tag
inverses = inverse <<< plain


inverse :: Tag -> Tag
inverse = Format Inverse


invisibles :: String -> Tag
invisibles = invisible <<< plain


invisible :: Tag -> Tag
invisible = Format Invisible


fgcs :: Color -> String -> Tag
fgcs c = fgc c <<< plain


fgc :: Color -> Tag -> Tag
fgc = FgC


fgs :: String -> String -> Tag
fgs cs = fg cs <<< plain


fg :: String -> Tag -> Tag
fg = Fg


bgcs :: Color -> String -> Tag
bgcs c = bgc c <<< plain


bgc :: Color -> Tag -> Tag
bgc = BgC


bgs :: String -> String -> Tag
bgs cs = bg cs <<< plain


bg :: String -> Tag -> Tag
bg = Bg


split :: Tag -> Tag -> Tag
split = Split


nl :: Tag
nl = Newline


nest :: Int -> Array Tag -> Tag
nest n = Nest $ Indent n


indent :: Int -> Tag -> Tag
indent n = nest n <<< singleton


group :: Array Tag -> Tag
group = nest 0


joinWith :: Tag -> Array Tag -> Tag
joinWith sep ts = s $ String.joinWith (render sep) $ render <$> ts


-- TODO: do syntax?

instance Show Tag where
    show = render


data OutputKind


foreign import data Blessed :: OutputKind
-- foreign import data OneLine :: OutputKind
-- foreign import data PlainText :: OutputKind
-- foreign import data Markdown :: OutputKind
foreign import data Html :: OutputKind


type Options = { oneLine :: Boolean, indent :: Int }


class Renderer (x :: OutputKind) where
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
    where
        wrap tag tagged = "{" <> tag <> "}" <> render tagged <> "{/" <> tag <> "}"