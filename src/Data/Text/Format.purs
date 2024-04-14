module Data.Text.Format where

import Prelude

import Color (Color)
import Color as Color

import Data.Array (singleton)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Either (Either(..)) as E
import Data.String (joinWith) as String
import Data.Time (Time)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, wrap, unwrap)

newtype Indent = Indent Int
newtype Level = Level Int
newtype FootnoteId = FootnoteId (Either Int String)
newtype Anchor = Anchor String
newtype ProgrammingLanguage = ProgrammingLanguage String
newtype Url = Url String
newtype Term = Term Tag
newtype Definition = Definition Tag


derive instance Newtype Indent _
derive instance Newtype Level _
derive instance Newtype FootnoteId _
derive instance Newtype Anchor _
derive instance Newtype ProgrammingLanguage _
derive instance Newtype Url _
derive instance Newtype Term _
derive instance Newtype Definition _


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
    | Link Url
    | Image Url
    | Footnote FootnoteId
    | LinkTo FootnoteId
    | Code ProgrammingLanguage
    | Define Term
    | Comment
    | Sub
    | Sup
    | Fg (Either String Color)
    | Bg (Either String Color)


-- data ListKind
--     = Ordered TheWay
--     | Unordered


data Bullet
    = None
    | Asterisk
    | Dash
    | Num
    | Circle
    | Alpha
    | AlphaInv
    -- ..


data Tag
    = Empty
    | Plain String
    | Format Format Tag
    | Align Align Tag
    | Split Tag Tag
    | Pair Tag Tag
    | Join Tag (Array Tag)
    | Para (Array Tag)
    | Nest Indent (Array Tag)
    | Newline
    | List Bullet Tag (Array Tag) -- The root `Tag`` is optional (if `Empty`) header of the list
    | Table (Array (Tag /\ Array Tag))
    | Hr

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
fgc = Format <<< Fg <<< E.Right


fgs :: String -> String -> Tag
fgs cs = fg cs <<< plain


fg :: String -> Tag -> Tag
fg = Format <<< Fg <<< E.Left


bgcs :: Color -> String -> Tag
bgcs c = bgc c <<< plain


bgc :: Color -> Tag -> Tag
bgc = Format <<< Bg <<< E.Right


bgs :: String -> String -> Tag
bgs cs = bg cs <<< plain


bg :: String -> Tag -> Tag
bg = Format <<< Bg <<< E.Left


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


bulletPrefix :: Int -> Bullet -> String
bulletPrefix index = case _ of
    None -> ""
    Asterisk -> "*"
    Dash -> "-"
    Circle -> "o" -- FIXME
    Alpha -> "a." -- FIXME: TODO
    AlphaInv -> "z." -- FIXME: TODO
    Num -> "1" -- FIXME: TODO


-- TODO: do syntax?

instance Show Tag where
    show = case _ of
        Empty -> just "empty"
        Plain str -> wrap "plain" str
        Align align tag -> wraparg "align" (show align) $ show tag
        Format format tag -> case format of
            Fg (E.Left colorStr) -> wraparg "fg" (show colorStr) $ show tag
            Fg (E.Right color) -> wraparg "fg" (show color) $ show tag
            Bg (E.Left colorStr) -> wraparg "bg" (show colorStr) $ show tag
            Bg (E.Right color) -> wraparg "bg" (show color) $ show tag
            Link (Url url) -> wraptag2 "link" (show tag) url
            LinkTo (FootnoteId ftnId) ->
                case ftnId of
                    E.Left ftnIntId -> wraptag2 "link" (show tag) $ wrap "ftn#" $ show ftnIntId
                    E.Right ftnStrId -> wraptag2 "link" (show tag) $ wrap "ftn" $ ftnStrId
            Define (Term term) -> let definition = tag in wraptag2 "def" (show term) $ show definition
            Image (Url url) -> let title = tag in wraparg "image" (show title) $ show url
            Comment -> wrap "comment" $ show tag
            Footnote (FootnoteId ftnId) ->
                case ftnId of
                    E.Left ftnIntId -> wraparg "footnote" ("#" <> show ftnIntId) $ show tag
                    E.Right ftnStrId -> wraparg "footnote" ftnStrId $ show tag
            _ -> wraparg "format" (show format) $ show tag
        Split tag1 tag2 -> wraptag2 "split" (show tag1) $ show tag2
        Pair tag1 tag2 -> wraptag2 "pair" (show tag1) $ show tag2
        Para tags -> wraplist "para" $ show <$> tags
        Newline -> just "nl"
        Hr -> just "hr"
        Nest indent tags -> wraplistarg "nest" (show indent) $ show <$> tags
        Join tag tags -> wraplistarg "join" (show tag) $ show <$> tags
        List bullet tag tags -> wraplistarg2 "list" (show bullet) (show tag) $ show <$> tags
        Table tags -> wrap "table" $ String.joinWith "|" $ uncurry tableitems <$> tags
        where
            just title = "(" <> title <> ")"
            wrap title v = "(" <> title <> ":" <> v <> ")"
            wraparg title arg v = "(" <> title <> "(" <> arg <> "):" <> v <> ")"
            wraptag2 title tag1 tag2 = "(" <> title <> ":" <> tag1 <> "," <> tag2 <> ")"
            wraplist title vals = "(" <> title <> ":" <> String.joinWith "," vals <> ")"
            wraplistarg title arg vals = "(" <> title <> "(" <> arg <> "):" <> String.joinWith "," vals <> ")"
            wraplistarg2 title arg1 arg2 vals = "(" <> title <> "(" <> arg1 <> "," <> arg2 <> "):" <> String.joinWith "," vals <> ")"
            tableitems t ts = show t <> ":" <> String.joinWith "," (show <$> ts)


instance Show Align where
    show Left = "left"
    show Right = "right"
    show Center = "center"


instance Show Bullet where
    show None = "none"
    show Asterisk = "asterisk"
    show Dash = "dash"
    show Circle = "circle"
    show Alpha = "alpha"
    show AlphaInv = "alphainv"
    show Num = "num"


instance Show Format where
    show = case _ of
        Bold -> "bold"
        Underline -> "underline"
        Blink -> "blink"
        Inverse -> "inverse"
        Invisible -> "invisible"
        Strikethrough -> "striked"
        Monospaced -> "mono"
        Quote -> "quote"
        Sub -> "sub"
        Sup -> "sup"
        Verbatim -> "verbatim"
        Footnote ftnId -> "footnote#" <> show ftnId
        Code lang -> "code:" <> show lang
        Header level anchor -> "header#" <> show level <> case anchor of
                                            Just anchor -> "{" <> show anchor <> "}"
                                            Nothing -> ""
        Link (Url url) -> "link:" <> show url
        Image (Url url) -> "image:" <> show url
        LinkTo (FootnoteId ftnId) -> case ftnId of
            E.Left intId -> "footnote:#" <> show intId
            E.Right strId -> "footnote:" <> strId
        Define (Term term) -> "define:" <> show term
        Comment -> "comment"
        Fg ecolor ->
            "fg(" <> case ecolor of
                    E.Left colorStr -> show colorStr
                    E.Right color -> show color <> ")"
        Bg ecolor ->
            "bg(" <> case ecolor of
                    E.Left colorStr -> show colorStr
                    E.Right color -> show color <> ")"
