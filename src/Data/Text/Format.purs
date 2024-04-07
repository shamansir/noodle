module Data.Text.Format where

import Prelude

import Color (Color)
import Color as Color

import Data.Array (singleton)
import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.Time (Time)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, wrap, unwrap)

newtype Indent = Indent Int
newtype Level = Level Int
newtype FootnoteId = FootnoteId Int
newtype Anchor = Anchor String
newtype ProgrammingLanguage = ProgrammingLanguage String


derive instance Newtype Indent _
derive instance Newtype Level _
derive instance Newtype FootnoteId _
derive instance Newtype Anchor _
derive instance Newtype ProgrammingLanguage _


derive newtype instance Show Indent
derive newtype instance Show Level
derive newtype instance Show FootnoteId
derive newtype instance Show Anchor
derive newtype instance Show ProgrammingLanguage



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
    | Strikethrough
    | Monospaced
    | Header Level (Maybe Anchor)
    | Quote
    | Verbatim
    | Footnote FootnoteId
    | Code ProgrammingLanguage
    | Sub
    | Sup


-- data ListKind
--     = Ordered TheWay
--     | Unordered


data TaskStatus
    = Done
    | Doing
    | Scheduled Date (Maybe Time)
    | ProgressPercent Int
    | ProgressOf Int Int
    | AutoProgress


data Tag
    = Empty
    | Plain String
    | FgC Color Tag
    | Fg String Tag
    | BgC Color Tag
    | Bg String Tag
    | Align Align Tag
    | Format Format Tag
    | Split Tag Tag
    | Pair Tag Tag
    | Nest Indent (Array Tag)
    | Join Tag (Array Tag)
    | Newline
    | Date Date
    | Time Time
    | List Tag (Array Tag) -- FIXME: homomorphic to join but the meaning is different
    | Table (Array (Tag /\ Array Tag))
    | Link Tag Tag
    | LinkTo Tag FootnoteId
    | Definition Tag Tag
    | Hr
    | Image Tag String
    | Property String String
    | Comment String
    | Macro String
    | Task TaskStatus Tag


-- TODO: binary operators for tags
-- TODO: empty tag


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
joinWith = Join


class Formatter a where
    format :: a -> Tag


instance Formatter Tag where
    format = identity


-- TODO: do syntax?

instance Show Tag where
    show = case _ of
        Empty -> just "empty"
        Plain str -> wrap "plain" str
        FgC color tag -> wraparg "fg" (show color) $ show tag
        Fg color tag -> wraparg "fg" (show color) $ show tag
        BgC color tag -> wraparg "bg" (show color) $ show tag
        Bg color tag -> wraparg "bg" (show color) $ show tag
        Align align tag -> wraparg "align" (show align) $ show tag
        Format format tag -> wraparg "format" (show format) $ show tag
        Split tag1 tag2 -> wraptag2 "split" (show tag1) $ show tag2
        Pair tag1 tag2 -> wraptag2 "pair" (show tag1) $ show tag2
        Newline -> just "nl"
        Hr -> just "hr"
        Link tag1 tag2 -> wraptag2 "link" (show tag1) $ show tag2
        Definition tag1 tag2 -> wraptag2 "def" (show tag1) $ show tag2
        Property name value -> wraptag2 "prop" name value
        Date date -> wrap "date" $ show date
        Time time -> wrap "time" $ show time
        Image title url -> wraparg "image" (show title) $ show url
        Comment str -> wrap "comment" str
        Macro str -> wrap "macro" str
        Task status tag -> wraparg "task" (show status) $ show tag
        Nest indent tags -> wraplistarg "nest" (show indent) $ show <$> tags
        Join tag tags -> wraplistarg "join" (show tag) $ show <$> tags
        List tag tags -> wraplistarg "list" (show tag) $ show <$> tags
        LinkTo tag ftnId -> wraparg "to" (show ftnId) $ show tag
        Table tags -> wrap "table" $ String.joinWith "|" $ uncurry tableitems <$> tags
        where
            just title = "(" <> title <> ")"
            wrap title v = "(" <> title <> ":" <> v <> ")"
            wraparg title arg v = "(" <> title <> "(" <> arg <> "):" <> v <> ")"
            wraptag2 title tag1 tag2 = "(" <> title <> ":" <> tag1 <> "," <> tag2 <> ")"
            -- wraplist title vals = "(" <> title <> ":" <> String.joinWith "," vals <> ")"
            wraplistarg title arg vals = "(" <> title <> "(" <> arg <> "):" <> String.joinWith "," vals <> ")"
            tableitems t ts = show t <> ":" <> String.joinWith "," (show <$> ts)


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
    show Strikethrough = "striked"
    show Monospaced = "mono"
    show Quote = "quote"
    show Sub = "sub"
    show Sup = "sup"
    show Verbatim = "verbatim"
    show (Footnote ftnId) = "footnote#" <> show ftnId
    show (Header level anchor) = "header#" <> show level <> case anchor of
                                            Just anchor -> "{" <> show anchor <> "}"
                                            Nothing -> ""
    show (Code lang) = "code:" <> show lang


instance Show TaskStatus where
    show = case _ of
        Done -> "DONE"
        Doing -> "DOING"
        Scheduled date time -> "AT" <> show date <> "::" <> show time
        ProgressPercent prc -> show prc <> "%"
        ProgressOf done total -> show done <> "/" <> show total
        AutoProgress -> "AUTO"