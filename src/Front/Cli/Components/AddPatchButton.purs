module Cli.Components.AddPatchButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Effect.Console (log) as Console

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed as B
import Blessed ((>~))
import Blessed.Internal.BlessedOp (lift') as Blessed

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Internal.Core as Core

import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys as Key
import Cli.State (State)
-- import Cli.State (patchIdFromIndex) as State
import Cli.Components.PatchesListbar as PatchesListbar
import Cli.Style as Style

import Noodle.Network as Network
import Noodle.Patch as Patch
import Noodle.Ui.Cli.Palette as Palette


component ∷ forall s fs r m. Core.Blessed (State s fs r m)
component =
    B.button Key.addPatchButton
        [ Box.content "+"
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> do
                state <- State.get
                let
                    patchesCount = state.network # Network.patchesCount
                    nextPatchIndex = patchesCount + 1
                nextPatch <- Blessed.lift' $ Patch.make ("Patch " <> show nextPatchIndex) state.initPatchesFrom
                let
                    nextNW = state.network # Network.addPatch nextPatch
                State.modify_
                    (_
                        { currentPatch = Just { index : nextPatchIndex, id : Patch.id nextPatch }
                        , network = nextNW
                        }
                    )
                PatchesListbar.updatePatches $ Network.patches nextNW -- TODO: load patches from state in PatchesBar, just call some refresh/update
                PatchesListbar.selectPatch nextPatchIndex
                -- TODO: clear the patches box content (ensure all the nodes and links are stored in the network for the previously selected patch)
                Key.mainScreen >~ Screen.render
        {-
        , Core.on Element.MouseOver
            \_ _ -> do
                liftEffect $ Console.log "over"
        , Core.on Element.MouseOut
            \_ _ -> do
                liftEffect $ Console.log "out"
        -}
        ]
        []