module Cli.Components.PatchesBar where

import Prelude

import Control.Monad.State as State

import Data.Maybe(Maybe(..))
import Data.Map (toUnfoldable) as Map
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\))

import Blessed as B
import Blessed ((>~), (~<))

import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Core.ListStyle as LStyle
import Blessed.Core.EndStyle as ES

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Lists.ListBar.Event as ListBar
import Blessed.UI.Lists.ListBar.Option as ListBar
import Blessed.UI.Lists.ListBar.Method as ListBar
import Blessed.UI.Base.Screen.Method as Screen

import Cli.Keys as Key
import Cli.Palette (palette)


-- Map Patch.Id (Patch gstate instances)
component patches =
    B.listbar Key.patchesBar
        [ Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.percents 100.0
        , Box.height $ Dimension.px 1
        , List.mouse true
        -- , List.items patches
        , ListBar.commands $ patchesLBCommands patches
        , List.style
            [ LStyle.bg palette.background
            , LStyle.item
                [ ES.fg palette.itemNotSelected
                , ES.bg palette.background
                ]
            , LStyle.selected
                [ ES.fg palette.itemSelected
                , ES.bg palette.background
                ]
            ]
        ]
        []


patchesLBCommands = mapWithIndex patchButton <<< Map.toUnfoldable


patchButton index (id /\ patch) =
    id /\ [] /\ \_ _ -> do
        State.modify_
            (_ { currentPatch = Just $ index /\ id })
        -- patchesBar >~ ListBar.selectTab index
        Key.mainScreen >~ Screen.render