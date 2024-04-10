module Data.Text.Doc2 where

import Prelude

import Data.Array ((:))
import Data.Array (replicate, uncons) as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith, length) as String
import Data.String.Extra (words) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (class Foldable)



data DOC
    = NIL
    | CONCAT DOC DOC
    | NEST Int DOC
    | TEXT String
    | JOIN DOC DOC
    | LINE



infixr 6 CONCAT as :<>
infixr 5 JOIN as :<|>


instance Semigroup DOC where
    append = CONCAT


data Doc
    = Nil
    | Text String Doc
    | Line Int Doc


nil :: DOC
nil = NIL
concat :: DOC -> DOC -> DOC
concat x y = x :<> y
nest :: Int -> DOC -> DOC
nest i x = NEST i x
text :: String -> DOC
text s = TEXT s
line :: DOC
line = LINE


group :: DOC -> DOC
group x = flatten x :<|> x


flatten :: DOC -> DOC
flatten NIL = NIL
flatten (CONCAT x y) = flatten x :<> flatten y
flatten (NEST i x) = NEST i $ flatten x
flatten (TEXT s) = TEXT s
flatten LINE = TEXT " "
flatten (JOIN x y) = flatten x


layout :: Doc -> String
layout Nil = ""
layout (Text s x) = s <> layout x
layout (Line i x) = "\n" <> (String.joinWith "" $ Array.replicate i " ") <> layout x


best :: Int -> Int -> DOC -> Doc
best w k x = be w k [ 0 /\ x ]


be :: Int -> Int -> Array (Int /\ DOC) -> Doc
be w k arr =
    case Array.uncons arr of
        Nothing -> Nil
        Just { head, tail } ->
            case head of
                _ /\ NIL -> be w k tail
                i /\ CONCAT x y -> be w k $ (i /\ x) : (i /\ y) : tail
                i /\ NEST j x -> be w k $ ((i + j) /\ x) : tail
                _ /\ TEXT s -> Text s $ be w (k + String.length s) tail
                i /\ LINE -> Line i $ be w i tail
                i /\ JOIN x y -> Line i $ better w k (be w k $ (i /\ x) : tail)
                                                     (be w k $ (i /\ y) : tail)


better :: Int -> Int -> Doc -> Doc -> Doc
better w k x y = if fits (w - k) x then x else y


fits :: Int -> Doc -> Boolean
fits w _ | w < 0 = false
fits _ Nil = true
fits w (Text s x) = fits (w - String.length s) x
fits _ (Line _ _) = true


pretty :: Int -> DOC -> String
pretty w x = layout $ best w 0 x


-- Utility

sp :: DOC -> DOC -> DOC
sp x y = x <> text " " <> y
br :: DOC -> DOC -> DOC
br x y = x <> line <> y
spbr :: DOC -> DOC -> DOC
spbr x y = x <> (text " " :<|> line) <> y


infixr 6 sp as <+>
infixr 6 br as </>
infixr 6 spbr as <+/>


folddoc :: (DOC -> DOC -> DOC) -> Array DOC -> DOC
folddoc f arr =
    case Array.uncons arr of
        Nothing -> nil
        Just { head, tail } ->
            case tail of
                [] -> head
                _ -> f head $ folddoc f tail


spread :: Array DOC -> DOC
spread = folddoc (<+>)
stack :: Array DOC -> DOC
stack = folddoc (</>)


bracket :: String -> DOC -> String -> DOC
bracket l x r = group $ text l <> nest 2 (line <> x) <> line <> text r


wbracket :: String -> String -> DOC -> DOC
wbracket l r x = bracket l x r


-- wbracket' :: DOC -> DOC -> DOC -> DOC
-- wbracket'


wrap :: String -> DOC -> DOC
wrap q = wbracket q q


mark :: String -> DOC -> DOC
mark s x = text s <+> x


fillwords :: String -> DOC
fillwords = folddoc (<+/>) <<< map text <<< String.words


fill :: Array DOC -> DOC
fill arr =
    case Array.uncons arr of
        Nothing -> nil
        Just { head, tail } ->
            case Array.uncons tail of
                Nothing -> head
                Just tailx ->
                    (flatten head <+> fill (flatten tailx.head : tailx.tail))
                    :<|>
                    (head </> fill (tailx.head : tailx.tail))


instance Show DOC where
    show = case _ of
        NIL -> "NIL"
        CONCAT docA docB -> "CONCAT(" <> show docA <> "," <> show docB <> ")"
        NEST n doc -> "NEST(" <> show n <> "," <> show doc <> ")"
        TEXT s -> "TEXT(" <> s <> ")"
        JOIN docA docB -> "JOIN(" <> show docA <> "," <> show docB <> ")"
        LINE -> "BR"


instance Show Doc where
    show = case _ of
        Nil -> "Nil"
        Text s doc -> "Text(" <> s <> "," <> show doc <> ")"
        Line n doc -> "Line(" <> show n <> "," <> show doc <> ")"