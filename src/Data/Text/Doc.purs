module Data.Text.Doc where

import Prelude

import Data.Array (intersperse, replicate) as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String (split, joinWith) as String


-- loosely based on https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

data Doc
    = Nil
    | Space
    | Indent
    | Text String
    | Break
    | Concat Doc Doc
    | Marker String Doc
    | Join Doc (Array Doc)
    | Para (Array Doc)
    | Wrap { start :: Doc, end :: Doc } Doc
    | Nest Int Doc
    -- TODO JoinDistribute


nil :: Doc
nil = Nil


s :: String -> Doc
s = Text


i :: Doc
i = Indent


sp :: Doc
sp = space


space :: Doc
space = Space


br :: Doc
br = Break


line :: String -> Doc
line str = s str <> br


l :: String -> Doc
l = line


break :: Doc
break = Break


mark :: String -> Doc -> Doc
mark = Marker


join :: Doc -> Array Doc -> Doc
join = Join


para :: Array Doc -> Doc
para = Para


wr :: Doc -> Doc -> Doc -> Doc
wr start end = Wrap { start, end }


wrap :: Doc -> Doc -> Doc -> Doc
wrap = wr


w :: String -> String -> Doc -> Doc
w start end = wr (s start <> sp) (sp <> s end)


ws :: String -> String -> Doc -> Doc
ws start end = wr (s start) (s end)


ww :: String -> Doc -> Doc
ww sym = ws sym sym


nest :: Int -> Doc -> Doc
nest = Nest


pair :: Doc -> Doc -> Doc
pair = Concat


data Indent
    = OneLiner
    | IndentWithSpaces Int
    | IndentWithTabs


data Colors
    = All
    | NoColors
    -- Palette?


type Options =
    { indent :: Indent
    -- , colors :: Colors
    , maxWidth :: Maybe Int
    }


instance Semigroup Doc where
    append :: Doc -> Doc -> Doc
    append = Concat


{-
layout :: Int -> Doc -> String
layout indent = case _ of
    Nil -> ""
    Space -> " "
    Break -> "\n"
    Indent -> mkIndent indent
    Text str ->
        case str # String.split (Pattern "\n") of
            [] -> ""
            [one] -> mkIndent indent <> one
            more -> layout indent $ Para $ Text <$> more
    Para docs ->
       String.joinWith "" (layout indent <$> Array.intersperse Break <$> docs)
    Concat docA docB -> layout indent docA <> layout 0 docB
    Marker str doc -> layout indent $ Text str <> Space <> doc
    Join sep docs -> layout indent $ Para $ Array.intersperse sep docs -- FIXME
    Wrap { start, end } doc -> layout indent $ Text $ layout 0 start <> layout 0 doc <> layout 0 end
    Nest nextIndent doc ->
        layout (indent + nextIndent) doc
    where
        -- raw = layout 0 -- TODO
        mkIndent n = String.joinWith "" $ Array.replicate (n * 4) " "
-}

indents :: Int -> Doc
indents = case _ of
    0 -> Nil
    n -> Concat Indent $ indents (n - 1)



layout :: Int -> Doc -> String
layout is = case _ of
    Nil -> ""
    Space -> " "
    Break -> "\n"
    Indent -> mkIndent is
    Text str ->
        case str # String.split (Pattern "\n") of
            [] -> ""
            [one] -> one
            more -> layout is $ Para $ Text <$> more
    Para docs ->
       String.joinWith "" $ raw <$> Array.intersperse (Break <> indents is) docs
    Concat docA docB -> raw docA <> raw docB
    Marker str doc -> raw $ Text str <> Space <> doc
    Join sep docs -> String.joinWith "" $ raw <$> Array.intersperse sep docs
    Wrap { start, end } doc -> raw $ Text $ raw start <> raw doc <> raw end
    Nest nis doc ->
        layout nis doc
    where
        raw doc = layout 0 doc
        mkIndent n = String.joinWith "" $ Array.replicate (n * 4) " "


-- raw :: Doc -> String
-- raw = layout 0


instance Show Doc where
    show :: Doc -> String
    show = case _ of
        Nil -> "<nil>"
        Space -> "<sp>"
        Break -> "<br>"
        Indent -> "<ind>"
        Text str ->
            "<text(" <> str <> ")>"
        Para docs ->
            "<para(" <> (String.joinWith "," $ show <$> docs) <> ")>"
        Concat docA docB ->
            "<concat(" <> show docA <> "," <> show docB <> ")>"
        Marker str doc ->
            "<marker(" <> show str <> "," <> show doc <> ")>"
        Join sep docs ->
            "<join(" <> show sep <> ",(" <> (String.joinWith "," $ show <$> docs) <> "))>"
        Wrap { start, end } doc ->
            "<wrap(" <> show start <> "," <> show end <> "," <> show doc <> ")>"
        Nest nextIndent doc ->
            "<nest(" <> show nextIndent <> "," <> show doc <> ")>"