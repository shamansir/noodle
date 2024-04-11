module Data.Text.Doc3 where

import Prelude

import Debug as Debug

import Data.String (joinWith) as String
import Data.Array ((:))
import Data.Array (mapWithIndex, intersperse, uncons) as Array
import Data.Maybe (Maybe(..))


data Doc
    = Nil
    | Text String
    | Break
    | Indent
    | Para (Array Doc)
    | Nest Int Doc
    | Pair Doc Doc


infixr 6 Nest as :<>


-- TODO: Indent size as an option
-- TODO: Word wrap
-- TODO: Doc with Value


{-
render :: Doc -> String
render = case _ of
    Nil -> ""
    Text s -> s
    Break -> "\n"
    Indent -> "<-->"
    Para docs -> String.joinWith "" $ render <$> Array.intersperse Break docs
    Nest n (Para docs) -> String.joinWith "" $ render <$> (mkIndent n : (Array.intersperse (brindent n) $ adapt n <$> docs))
    -- Nest _ (Nest m doc) -> render $ Pair (Pair Break $ mkIndent m) doc
    Nest n doc -> render $ Pair (mkIndent $ Debug.spy "n'" n) $ adapt n doc
    Pair a b -> render a <> render b
    where
        adapt n (Nest m x) = Nest m x -- Nest (max 0 $ m - n) x
        adapt n Break = brindent n
        adapt _ doc = doc
        brindent = Pair Break <<< mkIndent
        mkIndent 0 = Nil
        mkIndent n = Pair Indent $ mkIndent $ n - 1
-}


render' :: Int -> Doc -> String
render' level = case _ of
    Nil -> ""
    Text s -> s
    Break -> "\n"
    Indent -> "<-->"
    Para docs -> String.joinWith "" $ render <$> Array.intersperse (brIndent level) docs
    Nest n (Para docs) -> String.joinWith "" $ render <$> Array.mapWithIndex (adapt n) docs
    -- Nest _ (Nest m doc) -> render $ Pair (Pair Break $ mkIndent m) doc
    Nest n doc -> render $ adapt n 0 doc
    Pair a b -> render a <> render b
    where
        -- adapt n (Nest m x) = Nest (max 0 $ m - n + 1) $ adapt (max 0 $ m - n) x
        adapt _ 0 (Nest m x) = Nest m x
        adapt _ i (Nest m x) = Pair Break $ Nest m x
        adapt n i Break = brIndent n
        adapt n 0 doc = Pair (mkIndent n) doc
        adapt n i doc = Pair (brIndent n) doc
        brIndent = Pair Break <<< mkIndent
        mkIndent 0 = Nil
        mkIndent n = Pair Indent $ mkIndent $ n - 1


-- render :: Doc -> String
render :: Doc -> String
render doc = render' 0 doc


instance Semigroup Doc where
    append = Pair


text :: String -> Doc
text = Text
nil :: Doc
nil = Nil
break :: Doc
break = Break
nest :: Int -> Doc -> Doc
nest = Nest
stack :: Array Doc -> Doc
stack = Para
concat :: Doc -> Doc -> Doc
concat = Pair
indent :: Doc
indent = Indent


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


sp :: Doc -> Doc -> Doc
sp x y = x <> text " " <> y
br :: Doc -> Doc -> Doc
br x y = x <> break <> y
-- spbr :: Doc -> Doc -> Doc
-- spbr x y = x <> (break) <> y


infixr 6 sp as <+>
infixr 6 br as </>
-- infixr 6 spbr as <+/>


folddoc :: (Doc -> Doc -> Doc) -> Array Doc -> Doc
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