module Cli.Components.NodeBox.OutletButton where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Data.Maybe (Maybe(..))
import Data.Text.Output.Blessed (singleLine) as T

import Control.Monad.State (get, modify_) as State

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
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Base.Element.Method (show, focus) as Element

import Cli.Bounds (collect, outletPos) as Bounds
import Cli.Keys (OutletsBoxKey, OutletButtonKey, InfoBoxKey, NodeBoxKey, mainScreen)
import Cli.State (State) {- LinkState(..), OutletIndex(..), InputIndex(..), logNdfCommandM)  -}
import Cli.Style (inletsOutlets) as Style

import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.SidePanel.Console as CC

import Noodle.Id as Id
import Noodle.Patch (Patch)
import Noodle.Repr.ChRepr (ValueInChannel)
import Noodle.Ui.Cli.Tagging (outlet) as T
import Noodle.Ui.Cli.Tagging.At (class At, ChannelLabel, StatusLine) as T


--import Cli.Components.NodeBox.HasBody (class HasEditor, class HasEditor')



width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall tk pstate fs strepr chrepr m
     . T.At T.StatusLine chrepr
    => T.At T.ChannelLabel chrepr
    => OutletButtonKey -> NodeBoxKey -> InfoBoxKey
    -> Id.FamilyR -> Id.NodeR -> Id.OutletR
    -> Int
    -> ValueInChannel chrepr
    -> Signal (ValueInChannel chrepr)
    -- -> Raw.Node
    -> Core.Blessed (State tk pstate fs strepr chrepr m)
component buttonKey nodeBoxKey infoBoxKey familyR nodeR outletR idx vicRepr reprSignal =
    B.button buttonKey
        [ Box.content $ T.singleLine $ T.outlet idx outletR vicRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Button.Press
            $ onPress idx outletR nodeR nodeBoxKey
        , Core.on Element.MouseOver
            $ onMouseOver familyR nodeR nodeBoxKey infoBoxKey idx outletR vicRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut infoBoxKey idx
        ]
        []


onMouseOver
    :: forall tk pstate fs strepr chrepr m
     . T.At T.StatusLine chrepr
    => Id.FamilyR
    -> Id.NodeR
    -> NodeBoxKey
    -> InfoBoxKey
    -> Int
    -> Id.OutletR
    -> ValueInChannel chrepr
    -> Signal (ValueInChannel chrepr)
    -> _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr m) Effect
onMouseOver family nodeIdR nodeBox infoBox idx outletR vicRepr reprSignal _ _ = do
    state <- State.get
    nodeBounds <- Bounds.collect nodeIdR nodeBox
    let outletPos = Bounds.outletPos nodeBounds idx
    maybeRepr <- liftEffect $ Signal.get reprSignal
    infoBox >~ IB.outletInfo outletR
    SL.outletStatus family idx outletR vicRepr
    -- REM FI.outputStatus family idx outputId maybeRepr
    case state.lastClickedOutlet of
        Just _ -> pure unit
        Nothing -> do
            pure unit
            -- REM OI.move { x : outputPos.x, y : outputPos.y - 1 }
            -- REM OI.updateStatus OI.Hover
    mainScreen >~ Screen.render
    --CC.log $ "over" <> show idx


onMouseOut :: forall tk pstate fs strepr chrepr m. InfoBoxKey -> Int ->  _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr m) Effect
onMouseOut infoBox idx _ _ = do
    state <- State.get
    infoBox >~ IB.clear
    SL.clear
    -- REM: FI.clear
    case state.lastClickedOutlet of
        Just _ -> pure unit
        Nothing -> pure unit -- REM OI.hide
    mainScreen >~ Screen.render
    --CC.log $ "out" <> show idx


onPress :: forall tk pstate fs strepr chrepr m. Int -> Id.OutletR -> Id.NodeR -> NodeBoxKey -> _ -> _ -> BlessedOp (State tk pstate fs strepr chrepr m) Effect
onPress idx outletR nodeR nodeBoxKey _ _ = do
    CC.log "outlet press"
    nodeBounds <- Bounds.collect nodeR nodeBoxKey
    let outletPos = Bounds.outletPos nodeBounds idx
    -- REM OI.move { x : outputPos.x, y : outputPos.y - 1 }
    -- REM OI.updateStatus OI.WaitConnection
    mainScreen >~ Screen.render
    State.modify_
        (_
            { lastClickedOutlet =
                Just
                    { index : idx
                    , nodeKey : nodeBoxKey
                    , nodeId : nodeR
                    , outletId : outletR
                    }
            }
        )