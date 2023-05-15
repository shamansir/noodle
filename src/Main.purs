module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console as Console

import Control.Monad.State as State

import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Prim.Symbol (class Append) as S
import Data.Int (floor, toNumber)
import Data.Ord (abs)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Newtype (class Newtype, unwrap)
import Data.Map (Map)
import Data.Map as Map
import Data.Array ((:), (!!))
import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.List (toUnfoldable, length) as List
import Data.KeyHolder as KH
import Record.Extra (class Keys, keys) as Record
import Unsafe.Coerce (unsafeCoerce)
import Data.String as String
import Data.Repr (Repr, class FromRepr, class ToRepr, class FromToReprRow, toRepr, fromRepr)

import Blessed ((>~), (~<))
import Blessed (exit) as Blessed
import Blessed as B

import Blessed.Core.Border as Border
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.EndStyle as ES
import Blessed.Core.Key (Key) as C
import Blessed.Core.Key as Key
import Blessed.Core.ListStyle as LStyle
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style
import Blessed.Core.Orientation as Orientation

import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button)
import Blessed.Internal.Core as Core
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.BlessedOp (BlessedOpGet, BlessedOp, BlessedOpM)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>), RawNodeKey)
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.Emitter (class Fires) as E

import Blessed.UI.Base.Element.Event as Element
import Blessed.UI.Base.Element.Property as Element
import Blessed.UI.Base.Element.PropertySet as Element
import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Base.Screen as Screen
import Blessed.UI.Base.Screen.Event as Screen
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Event as List
import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Lists.List.Property as List
import Blessed.UI.Lists.ListBar.Event as ListBar
import Blessed.UI.Lists.ListBar.Option as ListBar
import Blessed.UI.Lists.ListBar.Method as ListBar
import Blessed.UI.Boxes.Line.Option as Line
import Blessed.UI.Boxes.Line.Event as Line
import Blessed.UI.Forms.Button as Button
import Blessed.UI.Forms.Button.Option as Button
import Blessed.UI.Forms.Button.Event as Button
-- import Blessed.UI.Line.Li ()

import Noodle.Id as Id
import Noodle.Toolkit3 as Toolkit
import Noodle.Network2 as Network
import Noodle.Network2 (Network) as Noodle
import Noodle.Patch4 as Patch
import Noodle.Patch4 (Patch) as Noodle
import Noodle.Node2 as Node
import Noodle.Node2 (Node) as Noodle
import Noodle.Family.Def as Family
import Noodle.Node2.MapsFolds.Repr (nodeToRepr, nodeToMapRepr, Repr(..), class HasRepr, class ToReprHelper) as R


import Cli.App as Cli
import Cli.Keys (NodeBoxKey)
import Cli.Keys as Key
import Cli.Palette (Palette, palette)
import Cli.Palette (fullInfo, toArray, pico8, hydraFns, x11colors, qitem, qitem') as Palette
import Cli.Style as Style
import Cli.State (initial, patchIdFromIndex) as State
import Cli.State (State, Link(..), InletIndex(..), OutletIndex(..))
import Cli.State.NwWraper (Network, wrapN, unwrapN, withNetwork)

import Cli.Components.Link as Link
import Cli.Components.PatchesBar as PatchesBar
import Cli.Components.PatchBox as PatchBox
import Cli.Components.AddPatch as AddPatch
import Cli.Components.Library as Library
import Cli.Components.MainScreen as MainScreen
import Cli.Components.PaletteList as PaletteList

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.BlessedRepr as Hydra


-- patches = [ "Patch 1", "Patch 2" ]
-- items = [ "foo", "bar", "buz", "hello", "lalala" ]


-- type Nodes = Hydra.Instances Effect


-- families :: Array Id.FamilyR
-- families = List.toUnfoldable $ Toolkit.nodeFamilies Hydra.toolkit


main1 :: Effect Unit
main1 =
  Cli.run State.initial MainScreen.component


-- ⊲ ⊳ ⋎ ⋏ ≺ ≻ ⊽ ⋀ ⋁ ∻ ∶ ∼ ∽ ∾ :: ∻ ∼ ∽ ≀ ⊶ ⊷ ⊸ ⋮ ⋯ ⋰ ⋱ ⊺ ⊢ ⊣ ⊤ ⊥ ⊦ ∣ ∤ ∥ ∦ ∗ ∘ ∙ ⋄ ⋅ ⋆ ⋇ > ⋁


testPalette :: Effect Unit
testPalette =
  Cli.run unit
    (B.screenAnd Key.mainScreen

        [ Screen.title "Palette"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ PaletteList.component 0 0
        ]

        $ \_ -> do
            Key.mainScreen >~ Screen.render
    )


main :: Effect Unit
-- main = main1
main = testPalette