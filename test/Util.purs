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

data CompareResult = Unknown | Match | DiffAt (Int /\ Int) | DiffSize (Int /\ Int) (Int /\ Int)

compareMultiline :: Array (Array CodePoint) -> Array (Array CodePoint) -> CompareResult
compareMultiline [] [] = Match
compareMultiline [] _ = DiffSize (0 /\ 0) (0 /\ 0)
compareMultiline _ [] = DiffSize (0 /\ 0) (0 /\ 0)
compareMultiline left right | Array.length left /= Array.length right = DiffSize (0 /\ 0) (0 /\ 0)
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
        Nothing -> DiffSize (0 /\ 0) (0 /\ 0)
    compareLines :: Int -> Array CodePoint -> Array CodePoint -> CompareResult
    compareLines y [] [] = Match
    compareLines y [] _  = DiffSize (0 /\ 0) (0 /\ 0)
    compareLines y _  [] = DiffSize (0 /\ 0) (0 /\ 0)
    compareLines y leftLine rightLine =
      Array.zip leftLine rightLine
        # foldrWithIndex (compareCPs y) Unknown
    compareCPs _ _ _ (DiffSize a b) = DiffSize a b
    compareCPs _ _ _ (DiffAt pos) = DiffAt pos
    compareCPs y x (l /\ r) _ =
      if (l == r) then Match
      else DiffAt (x /\ y)
