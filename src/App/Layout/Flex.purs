module App.Layout.Flex
  ( Flex
  )
  where


import Prelude

import App.Style.Order (Order)
import Control.Apply (lift2)
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Foldable (foldr)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd, curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Vec2 (Size, Size_, Pos, (<+>))
import Data.Vec2 as V2

import App.Layout.Flex.Axis
import App.Layout.Flex.Rule (Rule(..))


-- TODO: `IsLayout` instance (AutoSizedLayout?)


data Flex s a
    = Level (Axis s a)
    | Deeper (Axis s (Flex s a))