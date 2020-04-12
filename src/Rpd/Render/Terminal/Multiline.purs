module Rpd.Render.Terminal.Multiline where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex, foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, Pattern(..), codePointFromChar, fromCodePointArray, toCodePointArray)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))


infixl 8 Array.updateAt as >>


newtype Multiline = Multiline (Array Line)
type Line = Array CodePoint


getLines :: Multiline -> Array Line
getLines (Multiline ml) = ml


instance showMultiline :: Show Multiline where
    show (Multiline ml) =
        String.joinWith "\n" (map fromCodePointArray ml)


instance eqMultiline :: Eq Multiline where
    eq mll mlr =
        case compare mll mlr of
            Match -> true
            _ -> false


-- TODO: Foldable etc.

size :: Multiline -> Int /\ Int
size ml = cols ml /\ rows ml


rows :: Multiline -> Int
rows (Multiline lines) = Array.length lines


cols :: Multiline -> Int
cols = maxLineLength


maxLineLength :: Multiline -> Int
maxLineLength (Multiline ml) =
  foldr foldingF 0 ml
  where
    foldingF line prevMax = max prevMax $ Array.length line


data CompareResult
  = Unknown
  | Match
  | DiffAt (Int /\ Int) (CodePoint /\ CodePoint)
  | DiffSize (Int /\ Int) (Int /\ Int)


empty :: Multiline
empty = Multiline [[]]


empty' :: (Int /\ Int)  -> Multiline
empty' (width /\ height) =
    Multiline
        $ Array.replicate height
        $ Array.replicate width spaceCP
    where
        spaceCP = codePointFromChar ' '


from :: Array String -> Multiline
from = Multiline <<< map toCodePointArray


from' :: String -> Multiline
from' = Multiline <<< Array.singleton <<< toCodePointArray


clip :: (Int /\ Int) -> (Int /\ Int) -> Multiline -> Multiline
clip (x /\ y) (w /\ h) ml@(Multiline lines) =
    lines
        # Array.slice y h
        # map (Array.slice x w)
        # Multiline


shift :: (Int /\ Int) -> Multiline -> Multiline
shift (x /\ y) ml | x == 0 && y == 0 = ml
shift (x /\ y) ml@(Multiline lines) | otherwise =
    let
        (w /\ h) = size ml
        clipped = clip (x /\ y) ((w - x) /\ (y - h)) ml
    in
        empty' (w /\ h) # inject (x /\ y) clipped


place :: (Int /\ Int) -> String -> Multiline -> Multiline
place (x /\ y) string (Multiline lines) =
    Multiline $ fromMaybe lines $ do
        row <- lines !! y
        let row' = Array.mapWithIndex mapF row
        lines # y >> row'
    where
        strLen = String.length string
        strCP = String.toCodePointArray string
        mapF index cpoint
            | index < x = cpoint
            | index >= (x + strLen) = cpoint
            | otherwise = fromMaybe cpoint $ strCP !! (index - x)


place' :: (Int /\ Int) -> Char -> Multiline -> Multiline
place' (x /\ y) char (Multiline lines) =
    Multiline $ fromMaybe lines $ do
        row <- lines !! y
        row' <- row # x >> codePointFromChar char
        lines # y >> row'


inject :: (Int /\ Int) -> Multiline -> Multiline -> Multiline
inject (x /\ y) srcml@(Multiline srcLines) dstml@(Multiline destLines) =
    Multiline $ Array.mapWithIndex rowMapF destLines
    where
        ( srcW /\ scrH ) = size srcml
        rowMapF index dstLine | index < y = dstLine
        rowMapF index dstLine | index >= (y + scrH) = dstLine
        rowMapF index dstLine | otherwise =
            case srcLines !! (index - y) of
                Just srcLine -> Array.mapWithIndex (lineMapF srcLine) dstLine
                Nothing -> dstLine
        lineMapF srcLine index dstCpoint | index < x = dstCpoint
        lineMapF srcLine index dstCpoint | index >= (x + srcW) = dstCpoint
        lineMapF srcLine index dstCpoint | otherwise =
            fromMaybe dstCpoint $ srcLine !! (index - x)


clipWithMarker :: (Int /\ Int) -> (Int /\ Int) -> Multiline -> Multiline
clipWithMarker clipPos@(clipX /\ clipY) clipSize'@(clipW' /\ clipH') ml =
    let
        srcSize@(srcW /\ srcH) = size ml
        clipSize@(clipW /\ clipH) =
            (if srcW >= clipW' then clipW' else srcW) /\
            (if srcH >= clipH' then clipH' else srcH)
        bottomMarker =
            if srcW < clipW' then
                if srcH < clipH' then "⌟" else "|"
            else
                if srcH < clipH' then "-" else "+"
        coordsLabel = show clipX <> ":" <> show clipY <> " "
        topLeftX = String.length coordsLabel
        lineBelow = String.joinWith "" $ Array.replicate topLeftX "-"
        markerSize@(markerW /\ markerH) = (topLeftX + 1) /\ 2
        dstSize@(dstW /\ dstH) = (markerW + clipW + 1) /\ (clipH + markerH + 1)
        dst = empty' dstSize
    in
        dst
            # (inject ((topLeftX + 1) /\ markerH) $ clip clipPos clipSize ml)
            # (place (0 /\ 0) $ coordsLabel <> "|")
            # (place (0 /\ 1) $ lineBelow <> "+-")
            # place (topLeftX /\ 2) "|"
            # place ((dstW - 1) /\ (dstH - 1)) bottomMarker

toMultiline :: String -> Multiline
toMultiline source =
    Multiline
        $ map toCodePointArray
        $ String.split (Pattern "\n") source

-- codePointAt :: Int -> Int -> CodePoint


compare :: Multiline -> Multiline -> CompareResult
compare (Multiline []) (Multiline []) = Match
compare (Multiline []) right = DiffSize (0 /\ 0) $ size right
compare left (Multiline []) = DiffSize (size left) (0 /\ 0)
compare left right
    | rows left /= rows right = DiffSize (size left) (size right)
compare left@(Multiline leftLines) right@(Multiline rightLines)
    | otherwise =
  foldlWithIndex compareML Unknown leftLines
  where
    compareML :: Int -> CompareResult -> Line -> CompareResult
    compareML _ (DiffSize a b) _ = DiffSize a b
    compareML _ (DiffAt pos cps) _ = DiffAt pos cps
    compareML y _ leftLine =
      case rightLines !! y of
        Just rightLine ->
          compareLines y leftLine rightLine
        Nothing -> DiffSize (size left) (maxLineLength right /\ y)
    compareLines :: Int -> Line -> Line -> CompareResult
    compareLines y [] [] = Match
    compareLines y [] _ = DiffSize (size left) (size right)
    compareLines y _ [] = DiffSize (size left) (size right)
    compareLines y leftLine rightLine =
      Array.zip leftLine rightLine
        # foldlWithIndex (compareCPs y) Unknown
    compareCPs
        :: Int -> Int
        -> CompareResult
        -> CodePoint /\ CodePoint
        -> CompareResult
    compareCPs _ _ (DiffSize a b) _ = DiffSize a b
    compareCPs _ _ (DiffAt pos cps) _ = DiffAt pos cps
    compareCPs y x _ (l /\ r) =
      if (l == r) then Match
      else DiffAt (x /\ y) (l /\ r)


diffBlockSize :: Int /\ Int
diffBlockSize = 40 /\ 5


compare'
  :: Multiline
  -> Multiline
  -> CompareResult /\ Maybe (Multiline /\ Multiline)
compare' left right =
  case compare left right of
    DiffAt pos cps -> DiffAt pos cps /\
      Just (clipWithMarker pos diffBlockSize left /\
            clipWithMarker pos diffBlockSize right)
    DiffSize a b -> DiffSize a b /\
      Just (clipWithMarker (0 /\ 0) diffBlockSize left /\
            clipWithMarker (0 /\ 0) diffBlockSize right)
    status -> status /\ Nothing

