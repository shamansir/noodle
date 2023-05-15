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
import Cli.Palette (toArray) as Palette
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


selfNamedColors :: Array String
selfNamedColors = [ "red", "green", "blue", "yellow" ]


pcolor :: Int -> String -> Int -> Int -> Int -> String -> String /\ String
pcolor idx hex r g b name = hex /\ name


labeledColors :: Array (String /\ String)
labeledColors =
    [ pcolor 0 "#000000" 0 0 0 "black"
    , pcolor 1 "#1D2B53" 29 43 83 "dark-blue"
    , pcolor 2 "#7E2553" 126 37 83 "dark-purple"
    , pcolor 3 "#008751" 0 135 81 "dark-green"
    , pcolor 4 "#AB5236" 171 82 54 "brown"
    , pcolor 5 "#5F574F" 95 87 79 "dark-grey"
    , pcolor 6 "#C2C3C7" 194 195 199 "light-grey"
    , pcolor 7 "#FFF1E8" 255 241 232 "white"
    , pcolor 8 "#FF004D" 255 0 77 "red"
    , pcolor 9 "#FFA300" 255 163 0 "orange"
    , pcolor 10 "#FFEC27" 255 236 39 "yellow"
    , pcolor 11 "#00E436" 0 228 54 "green"
    , pcolor 12 "#29ADFF" 41 173 255 "blue"
    , pcolor 13 "#83769C" 131 118 156 "lavender"
    , pcolor 14 "#FF77A8" 255 119 168 "pink"
    , pcolor 15 "#FFCCAA" 255 204 170 "light-peach"
    , pcolor 128 "#291814" 41 24 20 "brownish-black"
    , pcolor 129 "#111D35" 17 29 53 "darker-blue"
    , pcolor 130 "#422136" 66 33 54 "darker-purple"
    , pcolor 131 "#125359" 18 83 89 "blue-green"
    , pcolor 132 "#742F29" 116 47 41 "dark-brown"
    , pcolor 133 "#49333B" 73 51 59 "darker-grey"
    , pcolor 134 "#A28879" 162 136 121 "medium-grey"
    , pcolor 135 "#F3EF7D" 243 239 125 "light-yellow"
    , pcolor 136 "#BE1250" 190 18 80 "dark-red"
    , pcolor 137 "#FF6C24" 255 108 36 "dark-orange"
    , pcolor 138 "#A8E72E" 168 231 46 "lime-green"
    , pcolor 139 "#00B543" 0 181 67 "medium-green"
    , pcolor 140 "#065AB5" 6 90 181 "true-blue"
    , pcolor 141 "#754665" 117 70 101 "mauve"
    , pcolor 142 "#FF6E59" 255 110 89 "dark-peach"
    , pcolor 143 "#FF9D81" 255 157 129 "peach"
    ]



testPalette :: Palette -> Effect Unit
testPalette palette =
    let
        paletteKey = (nk :: List <^> "palette")
        colorToPair c = c /\ c
        pairToListItem (color /\ title) = "{" <> color <> "-bg}      {/" <> color <> "-bg} {" <> color <> "-fg}" <> title <> "{/" <> color <> "-fg}"
        paletteComp =
            B.list paletteKey
                [ Box.width $ Dimension.percents 40.0
                , Box.height $ Dimension.percents 100.0
                , Box.top $ Offset.px 0
                , Box.left $ Offset.px 0
                , List.items $ pairToListItem <$> (("white" /\ "title") : (colorToPair <$> selfNamedColors) <> labeledColors <> Palette.toArray palette)
                , List.mouse true
                , List.keys true
                , Box.tags true
                ]
                [ ]
  in Cli.run unit
    (B.screenAnd Key.mainScreen

        [ Screen.title "Palette"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ paletteComp
        ]

        $ \_ -> do
            Key.mainScreen >~ Screen.render
    )


main :: Effect Unit
-- main = main1
main = testPalette palette