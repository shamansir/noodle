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
import Cli.Palette (palette)
import Cli.Style as Style
import Cli.State (initial, patchIdFromIndex) as State
import Cli.State (State, Link(..), InletIndex(..), OutletIndex(..))
import Cli.State.NwWraper (Network, wrapN, unwrapN, withNetwork)

import Cli.Components.Link as Link
import Cli.Components.PatchesBar as PatchesBar
import Cli.Components.AddPatch as AddPatch
import Cli.Components.Library as Library

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.BlessedRepr as Hydra


-- patches = [ "Patch 1", "Patch 2" ]
-- items = [ "foo", "bar", "buz", "hello", "lalala" ]


-- type Nodes = Hydra.Instances Effect


families :: Array Id.FamilyR
families = List.toUnfoldable $ Toolkit.nodeFamilies Hydra.toolkit


main1 :: Effect Unit
main1 =
  Cli.run State.initial
    (B.screenAnd Key.mainScreen

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ PatchesBar.component $ Network.patches $ unwrapN State.initial.network

        , B.box Key.patchBox

            [ Box.top $ Offset.calc $ Coord.center <+> Coord.px 1
            , Box.left $ Offset.center
            , Box.width $ Dimension.percents 100.0
            , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 1
            , Box.content "Patch"
            , Box.tags true
            , Box.border
                [ Border.type_ Border._line
                ]
            , Box.style
                [ Style.fg palette.foreground
                , Style.bg palette.background2
                , Style.border [ Border.fg palette.border ]
                ]
            ]

            [ Library.component families
            ]

        , AddPatch.component
        ]


        $ \_ -> do
            PatchesBar.selectPatch 1
            Key.nodeList >~ Box.focus
            Key.mainScreen >~ Screen.render
        )

    where

        familyButton index (id /\ family) =
            id /\ [] /\ \_ _ -> do
                -- State.modify_
                --     (_ { currentPatch = Just $ index /\ id })
                -- patchesBar >~ ListBar.selectTab index
                -- TODO: try Toolkit.unsafeSpawnR
                Key.mainScreen >~ Screen.render


-- ⊲ ⊳ ⋎ ⋏ ≺ ≻ ⊽ ⋀ ⋁ ∻ ∶ ∼ ∽ ∾ :: ∻ ∼ ∽ ≀ ⊶ ⊷ ⊸ ⋮ ⋯ ⋰ ⋱ ⊺ ⊢ ⊣ ⊤ ⊥ ⊦ ∣ ∤ ∥ ∦ ∗ ∘ ∙ ⋄ ⋅ ⋆ ⋇ > ⋁


main2 :: Effect Unit
main2 =
    let
        lbKey = (nk :: ListBar <^> "test")
        inletHandler iname = iname /\ [ ] /\ \_ _ -> do liftEffect $ Console.log $ "cmd " <> iname
        inletsBarN =
            B.listbar lbKey
                [ Box.width $ Dimension.percents 90.0
                , Box.height $ Dimension.px 1
                , Box.top $ Offset.px 0
                , Box.left $ Offset.px 0
                , ListBar.commands $ inletHandler <$> [ "a", "b", "c" ]
                , List.mouse true
                , List.keys true
                , ListBar.autoCommandKeys true
                , Style.inletsOutlets
                , Core.on ListBar.Select
                    \_ _ -> do
                        -- liftEffect $ Console.log "inlet"
                        inletSelected <- List.selected ~< lbKey
                        -- liftEffect $ Console.log $ show inletSelected
                        pure unit
                -- FIXME: this way it is only possible to assign to one type of events
                -- FIXME: make all B.listBar, B.box methods and so on return object with `Blessed.Event == Blessed.CoreEvent`
                -- FIXME: and/or don't restrict Blessed.* methods to particular events
                , Core.on Element.Click
                    \_ _ -> do
                        liftEffect $ Console.log "preassigned click"
                ]
                [ ]
        nbKey = (nk :: Box <^> "node-box")
        testNodeBox =
            B.box nbKey
                [ Box.draggable true
                , Box.top $ Offset.px 10
                , Box.left $ Offset.px 10
                , Box.width $ Dimension.px 25
                , Box.height $ Dimension.px 5
                , Box.border
                    [ Border.type_ Border._line
                    , Border.fg palette.nodeBoxBorder
                    , Border.ch $ Border.fill ':'
                    ]
                , Box.style
                    [ Style.focus -- FIXME: makes it fail on drag
                        [ ES.border
                            [ Border.fg palette.nodeListSelFg
                            ]
                        ]
                    ]
                    -- []
                ]
                [ ]
  in Cli.run State.initial
    (B.screenAnd Key.mainScreen

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ inletsBarN
        , testNodeBox
        ]

        $ \_ -> do
            lbKey >~ ListBar.setItems
                            [ "test1" /\ [] /\ \_ _ -> do liftEffect $ Console.log "foo"
                            , "test2" /\ [] /\ \_ _ -> do liftEffect $ Console.log "bar"
                            ]
            lbKey >~ ListBar.addItem "test4" []
            lbKey >~ ListBar.addItemH "test3" [] \_ _ -> do liftEffect $ Console.log "buz"
            lbKey >~ Core.on' ListBar.Select $ \_ _ -> liftEffect $ Console.log "click assigned after"
            lbKey >~ Core.on' Element.Move $  \_ _ -> liftEffect $ Console.log "click assigned after"
            Key.mainScreen >~ Screen.render
    )

-- FIXME: removing links isn't working yet, click works now


main :: Effect Unit
main = main1