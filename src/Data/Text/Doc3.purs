module Data.Text.Doc3 where

import Prelude

import Data.String (joinWith) as String
import Data.Array ((:))
import Data.Array (intersperse, uncons) as Array
import Data.Maybe (Maybe(..))


type DOC = Doc


data Doc
    = Nil
    | Text String
    | Break
    | Indent
    | Para (Array Doc)
    | Nest Int Doc
    | Pair Doc Doc


render :: Doc -> String
render = case _ of
    Nil -> ""
    Text s -> s
    Break -> "\n"
    Indent -> "    "
    Para docs -> String.joinWith "" $ render <$> Array.intersperse Break docs
    Nest n (Para docs) -> String.joinWith "" $ render <$> (mkIndent n : (Array.intersperse (brindent n) $ adapt n <$> docs))
    -- Nest _ (Nest m doc) -> render $ Pair (Pair Break $ mkIndent m) doc
    Nest n doc -> render $ Pair (mkIndent n) $ adapt n doc
    Pair a b -> render a <> render b
    where
        adapt n (Nest m x) = Nest (max 0 $ m - n) x
        adapt n Break = brindent n
        adapt _ doc = doc
        brindent = Pair Break <<< mkIndent
        mkIndent 0 = Nil
        mkIndent n = Pair Indent $ mkIndent $ n - 1


instance Semigroup Doc where
    append = Pair


text :: String -> Doc
text = Text
nil :: Doc
nil = Nil
line :: Doc
line = Break
nest :: Int -> Doc -> Doc
nest = Nest
stack :: Array Doc -> Doc
stack = Para
concat :: Doc -> Doc -> Doc
concat = Pair


space :: Doc
space = Text " "
mark :: String -> Doc -> Doc
mark s doc = Text s <> space <> doc
bracket :: String -> Doc -> String -> Doc
bracket l x r = Text l <> x <> Text r
wbracket :: String -> String -> Doc -> Doc
wbracket l = flip $ bracket l
wrap :: String -> Doc -> Doc
wrap q = wbracket q q


pretty :: Int -> Doc -> String
pretty w = render
group :: Doc -> Doc
group = identity
fill :: Array Doc -> Doc
fill = stack


folddoc :: (DOC -> DOC -> DOC) -> Array DOC -> DOC
folddoc f arr =
    case Array.uncons arr of
        Nothing -> nil
        Just { head, tail } ->
            case tail of
                [] -> head
                _ -> f head $ folddoc f tail


instance Show Doc where
    show :: Doc -> String
    show = case _ of
        Nil -> "<nil>"
        -- Space -> "<sp>"
        Break -> "<br>"
        Indent -> "<ind>"
        Text str ->
            "<text(" <> str <> ")>"
        Para docs ->
            "<para(" <> (String.joinWith "," $ show <$> docs) <> ")>"
        Pair docA docB ->
            "<concat(" <> show docA <> "," <> show docB <> ")>"
        -- Marker str doc ->
        --     "<marker(" <> show str <> "," <> show doc <> ")>"
        -- Join sep docs ->
        --     "<join(" <> show sep <> ",(" <> (String.joinWith "," $ show <$> docs) <> "))>"
        -- Wrap { start, end } doc ->
        --     "<wrap(" <> show start <> "," <> show end <> "," <> show doc <> ")>"
        Nest nextIndent doc ->
            "<nest(" <> show nextIndent <> "," <> show doc <> ")>"