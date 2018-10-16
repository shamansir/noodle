module Rpd.Renderer.Terminal.Multiline where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.String (CodePoint, fromCodePointArray, toCodePointArray, codePointFromChar)
import Data.String as String
import Data.Array as Array
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)


infixl 8 Array.insertAt as >>


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
  | DiffAt (Int /\ Int)
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


clipWithMarker :: (Int /\ Int) -> (Int /\ Int) -> Multiline -> Multiline
clipWithMarker pos@(x /\ y) size@(w /\ h) ml =
    let
        markerSize@(markerW /\ markerH) = 10 /\ 4
        dstSize = (w + markerW) /\ (h + markerH)
        dst = empty' dstSize
    in
        dst
            # (inject markerSize $ clip pos size ml)
            # (place (0 /\ 0) $ show x <> ":" <> show y <> "|")
            # (place (0 /\ 1) $ "------|")


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
            | index > (x + strLen) = cpoint
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
        rowMapF index dstLine | index > (y + scrH) = dstLine
        rowMapF index dstLine | otherwise =
            case srcLines !! (index - y) of
                Just srcLine -> Array.mapWithIndex (lineMapF srcLine) dstLine
                Nothing -> dstLine
        lineMapF srcLine index dstCpoint | index < x = dstCpoint
        lineMapF srcLine index dstCpoint | index > (x + srcW) = dstCpoint
        lineMapF srcLine index dstCpoint | otherwise =
            fromMaybe dstCpoint $ srcLine !! (index - x)


-- inject :: Pos -> Bounds -> Multiline -> Multiline -> Multiline
-- inject pos bounds what into =
--     Array.mapWithIndex mapRow into
--     where
--         startCol = pos.x
--         startRow = pos.y
--         width = bounds.width
--         height = bounds.height
--         mapRow rowIdx row = Array.mapWithIndex (mapCol rowIdx) row
--         mapCol rowIdx colIdx cpoint | rowIdx < startRow || colIdx < startCol = cpoint
--         mapCol rowIdx colIdx cpoint | rowIdx >= (startRow + height)
--                                       || colIdx >= (startCol + width) = cpoint
--         mapCol rowIdx colIdx cpoint | otherwise =
--             fromMaybe cpoint $ do
--                 whatRow <- what !! (rowIdx - startRow)
--                 whatCp <- whatRow !! (colIdx - startCol)
--                 pure whatCp



-- codePointAt :: Int -> Int -> CodePoint


compare :: Multiline -> Multiline -> CompareResult
compare (Multiline []) (Multiline []) = Match
compare (Multiline []) right = DiffSize (0 /\ 0) $ size right
compare left (Multiline []) = DiffSize (size left) (0 /\ 0)
compare left right
    | rows left /= rows right = DiffSize (size left) (size right)
compare left@(Multiline leftLines) right@(Multiline rightLines)
    | otherwise =
  foldrWithIndex compareML Unknown leftLines
  where
    compareML :: Int -> Line -> CompareResult -> CompareResult
    compareML _ _ (DiffSize a b) = DiffSize a b
    compareML _ _ (DiffAt pos) = DiffAt pos
    compareML y leftLine _ =
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
        # foldrWithIndex (compareCPs y) Unknown
    compareCPs _ _ _ (DiffSize a b) = DiffSize a b
    compareCPs _ _ _ (DiffAt pos) = DiffAt pos
    compareCPs y x (l /\ r) _ =
      if (l == r) then Match
      else DiffAt (x /\ y)


compare'
  :: Multiline
  -> Multiline
  -> CompareResult /\ Maybe (Multiline /\ Multiline)
compare' left right =
  case compare left right of
    DiffAt pos -> DiffAt pos /\
      Just (clipWithMarker pos (5 /\ 5) left /\
            clipWithMarker pos (5 /\ 5) right)
    status -> status /\ Nothing

