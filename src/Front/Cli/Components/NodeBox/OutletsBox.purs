module Cli.Components.NodeBox.OutletsBox where

import Prelude

import Effect (Effect)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe)
import Data.Array (length, zip) as Array
import Data.Map (Map)
import Data.Map as Map
import Signal (Signal)
import Signal as Signal

import Blessed as B

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.NodeKey (nestChain) as NK

import Blessed.UI.Boxes.Box.Option as Box

import Cli.Keys (NodeBoxKey, OutletsBoxKey, OutletButtonKey, InfoBoxKey)
import Cli.Style as Style
import Cli.State (State)
import Cli.Components.NodeBox.OutletButton as OutletButton

import Noodle.Id as Id
import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Patch as Patch


width :: Int -> Dimension
width = Dimension.px <<< widthN


widthN :: Int -> Int
widthN count = (OutletButton.widthN + 1) * count