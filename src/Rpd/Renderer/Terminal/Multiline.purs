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


instance showMultiline :: Show Multiline where
    show (Multiline ml) =
        String.joinWith "\n" (map fromCodePointArray ml)


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


empty' :: { width :: Int, height :: Int } -> Multiline
empty' { width, height } =
    Multiline
        $ Array.replicate height
        $ Array.replicate width spaceCP
    where
        spaceCP = codePointFromChar ' '


from :: String -> Multiline
from str =
    Multiline $ Array.singleton $ toCodePointArray str


place :: { x :: Int, y :: Int } -> String -> Multiline -> Multiline
place pos string (Multiline ml) =
    Multiline $ fromMaybe ml $ do
        row <- ml !! pos.y
        let row' = Array.mapWithIndex mapF row
        ml # pos.y >> row'
    where
        strLen = String.length string
        strCP = String.toCodePointArray string
        startAt = pos.x
        mapF index cpoint
            | index < startAt = cpoint
            | index > (startAt + strLen) = cpoint
            | otherwise = fromMaybe cpoint $ strCP !! (index - startAt)


place' :: { x :: Int, y :: Int } -> Char -> Multiline -> Multiline
place' pos char (Multiline ml) =
    Multiline $ fromMaybe ml $ do
        row <- ml !! pos.y
        row' <- row # pos.x >> codePointFromChar char
        ml # pos.y >> row'


-- codePointAt :: Int -> Int -> Multiline


compareMultiline :: Multiline -> Multiline -> CompareResult
compareMultiline (Multiline []) (Multiline []) = Match
compareMultiline (Multiline []) right = DiffSize (0 /\ 0) $ size right
compareMultiline left (Multiline []) = DiffSize (size left) (0 /\ 0)
compareMultiline left right
    | rows left /= rows right = DiffSize (size left) (size right)
compareMultiline left@(Multiline leftLines) right@(Multiline rightLines)
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


compareMultiline'
  :: Multiline
  -> Multiline
  -> CompareResult /\ Maybe (Multiline /\ Multiline)
compareMultiline' left right =
  case compareMultiline left right of
    DiffAt pos -> DiffAt pos /\
      Just (clip left pos (5 /\ 5) /\
            clip right pos (5 /\ 5))
    status -> status /\ Nothing
  where
    clip :: Multiline -> (Int /\ Int) -> (Int /\ Int) -> Multiline
    clip v (x /\ y) (w /\ h) =
      let
        status1 = show x <> ":" <> show y <> "|"
        status2 = "-------+"
      in
        empty -- TODO
