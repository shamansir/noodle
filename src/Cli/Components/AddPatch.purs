module Cli.Components.AddPatch where

import Prelude


import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Internal.Core as Core

import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box

import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys as Key
import Cli.Palette (palette)
import Cli.State (State)
import Cli.State (patchIdFromIndex) as State
import Cli.State.NwWraper (unwrapN, withNetwork)
import Cli.Components.PatchesBar as PatchesBar

import Noodle.Network2 as Network
import Noodle.Patch4 as Patch

import Toolkit.Hydra2 as Hydra


component ∷ Core.Blessed State
component =
    B.button Key.addPatchButton
            [ Box.content "+"
            , Box.top $ Offset.px 0
            , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
            , Box.width $ Dimension.px 1
            , Box.height $ Dimension.px 1
            , Button.mouse true
            , Box.style
                [ Style.fg palette.foreground
                , Style.bg palette.background2
                ]
            , Core.on Button.Press
                \_ _ -> do
                    let nextPatch = Patch.init Hydra.toolkit
                    state <- State.get
                    let
                        patchesCount = unwrapN state.network # Network.patchesCount
                        patchNumId = patchesCount
                        patchId = State.patchIdFromIndex patchNumId
                        nextNW = state.network # withNetwork (Network.addPatch patchId nextPatch)
                    State.modify_
                        (_
                            { currentPatch = Just $ patchNumId /\ patchId
                            , network = nextNW
                            }
                        )
                    PatchesBar.updatePatches $ Network.patches $ unwrapN nextNW -- TODO: load patches from state in PatchesBar, just call some refresh/update
                    PatchesBar.selectPatch patchNumId
                    -- TODO: clear the patches box content (ensure all the nodes and links are stored in the network for the previously selected patch)
                    Key.mainScreen >~ Screen.render
            ]
            []