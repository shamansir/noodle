module Cli.Components.NodeBox.InletButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Control.Monad.State (get) as State

import Data.Maybe (Maybe(..))
import Data.Text.Output.Blessed (singleLine) as T

import Signal (Signal)
import Signal (get) as Signal

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Base.Element.Method (show, focus) as Element


import Cli.Bounds (collect, inletPos) as Bounds
import Cli.Keys (InfoBoxKey, InletButtonKey, NodeBoxKey, mainScreen)
import Cli.State (State) {- LinkState(..), OutletIndex(..), InputIndex(..), logNdfCommandM)  -}
import Cli.Style (inletsOutlets) as Style

import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL

import Noodle.Ui.Cli.Tagging (inlet) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T
import Noodle.Id as Id
import Noodle.Patch (Patch)


--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall tk pstate fs repr m
     . T.At T.StatusLine repr
    => T.At T.ChannelLabel repr
    => Patch pstate fs repr m
    -> InletButtonKey -> NodeBoxKey -> InfoBoxKey
    -> Id.FamilyR -> Id.NodeR -> Id.InletR
    -> Int
    -> Maybe repr
    -> Signal repr
    -- -> Raw.Node
    -> Core.Blessed (State tk pstate fs repr m)
component curPatch buttonKey nodeBoxKey infoBoxKey familyR nodeR inletR idx mbRepr reprSignal =
    B.button buttonKey
        [ Box.content $ T.singleLine $ T.inlet idx inletR mbRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        -- REM , Core.on Button.Press
        -- REM     $ onPress curPatchId curPatch nextNodeBox idx pdin inode inletId $ Hydra.editorIdOf =<< maybeRepr

        , Core.on Element.MouseOver
            $ onMouseOver familyR nodeR nodeBoxKey infoBoxKey idx inletR mbRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut infoBoxKey idx
        ]
        []


onMouseOver
    :: forall tk pstate fs repr m
     . T.At T.StatusLine repr
    => Id.FamilyR
    -> Id.NodeR
    -> NodeBoxKey
    -> InfoBoxKey
    -> Int
    -> Id.InletR
    -> Maybe repr
    -> Signal repr
    -> _ -> _ -> BlessedOp (State tk pstate fs repr m) Effect
onMouseOver familyR nodeIdR nodeBox infoBox idx inletR mbRepr reprSignal _ _ = do
    state <- State.get
    nodeBounds <- Bounds.collect nodeIdR nodeBox -- FIXME: load from state.locations
    let inletPos = Bounds.inletPos nodeBounds idx
    maybeRepr <- liftEffect $ Signal.get reprSignal
    infoBox >~ IB.inletInfo inletR
    SL.inletStatus familyR idx inletR mbRepr
    -- REM FI.inletStatus family idx inletId maybeRepr
    case state.lastClickedOutput of
        Just _ -> pure unit
        Nothing -> do
            pure unit
            -- REM II.move { x : inletPos.x, y : inletPos.y - 1 }
            -- REM II.updateStatus II.Hover
    mainScreen >~ Screen.render


onMouseOut :: forall tk pstate fs repr m. InfoBoxKey -> Int ->  _ -> _ -> BlessedOp (State tk pstate fs repr m) Effect
onMouseOut infoBox idx _ _ = do
    state <- State.get
    infoBox >~ IB.clear
    SL.clear
    -- REM FI.clear
    case state.lastClickedOutput of
        Just _ -> pure unit
        Nothing -> pure unit -- REM II.hide
    mainScreen >~ Screen.render