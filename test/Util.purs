module RpdTest.Util
    ( runWith
    , withRpd
    ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.String (CodePoint, fromCodePointArray, toCodePointArray, codePointFromChar)
import Data.String as String
import Data.Array as Array
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldr)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.FoldableWithIndex (foldrWithIndex)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import Rpd.API (Rpd) as Rpd
import Rpd.Network (Network) as Rpd
import Rpd.Network (empty) as Network
import Rpd.Log (runRpdLogging) as RL

runWith :: forall d. Rpd.Network d -> (Rpd.Network d -> Aff Unit) -> Aff Unit
runWith initialNetwork f = f initialNetwork

-- runWith :: forall e d. Rpd.Network d -> (Rpd.Network d -> TestAff e) -> TestAff e
-- runWith initialNetwork f = do
--   newNetwork <- liftEff $ do
--     networkRef <- newRef Rpd.empty
--     Rpd.run (writeRef networkRef) initialNetwork
--     readRef networkRef
--   f newNetwork

withRpd
  :: forall d
   . (Rpd.Network d -> Aff Unit)
  -> Rpd.Rpd (Rpd.Network d)
  -> Aff Unit
withRpd test rpd =
  liftEffect (getNetwork rpd) >>= test
  where
    --getNetwork :: R.Rpd d e -> R.RpdEff e (R.Network d e)
    getNetwork rpd = do
      nwTarget <- Ref.new $ Network.empty "f"
      _ <- RL.runRpdLogging (flip Ref.write $ nwTarget) rpd
      Ref.read nwTarget

data CompareResult
  = Unknown
  | Match
  | DiffAt (Int /\ Int)
  | DiffSize (Int /\ Int) (Int /\ Int)


sizeOf :: Array (Array CodePoint) -> Int /\ Int
sizeOf multiline =
  maxLen multiline /\ Array.length multiline


maxLen :: Array (Array CodePoint) -> Int
maxLen multine =
  foldr foldingF 0 multine
  where
    foldingF line prevMax = max prevMax $ Array.length line


compareMultiline :: Array (Array CodePoint) -> Array (Array CodePoint) -> CompareResult
compareMultiline [] [] = Match
compareMultiline [] right = DiffSize (0 /\ 0) $ sizeOf right
compareMultiline left [] = DiffSize (sizeOf left) (0 /\ 0)
compareMultiline left right | Array.length left /= Array.length right =
  DiffSize (sizeOf left) (sizeOf right)
compareMultiline left right | otherwise =
  foldrWithIndex compareML Unknown left
  where
    compareML :: Int -> Array CodePoint -> CompareResult -> CompareResult
    compareML _ _ (DiffSize a b) = DiffSize a b
    compareML _ _ (DiffAt pos) = DiffAt pos
    compareML y leftLine _ =
      case right !! y of
        Just rightLine ->
          compareLines y leftLine rightLine
        Nothing -> DiffSize (sizeOf left) (maxLen right /\ y)
    compareLines :: Int -> Array CodePoint -> Array CodePoint -> CompareResult
    compareLines y [] [] = Match
    compareLines y [] _  = DiffSize (sizeOf left) (sizeOf right)
    compareLines y _  [] = DiffSize (sizeOf left) (sizeOf right)
    compareLines y leftLine rightLine =
      Array.zip leftLine rightLine
        # foldrWithIndex (compareCPs y) Unknown
    compareCPs _ _ _ (DiffSize a b) = DiffSize a b
    compareCPs _ _ _ (DiffAt pos) = DiffAt pos
    compareCPs y x (l /\ r) _ =
      if (l == r) then Match
      else DiffAt (x /\ y)


compareMultiline'
  :: Array (Array CodePoint)
  -> Array (Array CodePoint)
  -> CompareResult /\ Maybe (Array (Array CodePoint) /\ Array (Array CodePoint))
compareMultiline' left right =
  case compareMultiline left right of
    DiffAt pos -> DiffAt pos /\ Nothing
      -- FIXME: add mismatching views
    status -> status /\ Nothing
