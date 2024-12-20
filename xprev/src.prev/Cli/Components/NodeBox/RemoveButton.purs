module Cli.Components.NodeBox.RemoveButton where

import Prelude


import Effect (Effect)

import Data.Symbol (class IsSymbol)
import Data.Tuple as Tuple
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (for_)
import Data.Map (lookup, empty) as Map
import Data.Text.Output.Blessed (singleLine) as T

import Control.Monad.State (get, modify_) as State

import Blessed as B

import Blessed ((>~))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NodeKey
import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys (PatchBoxKey, NodeBoxKey, RemoveButtonKey, InfoBoxKey)
import Cli.Keys (mainScreen, statusLine) as Key
import Cli.Style as Style
import Cli.Tagging as T
import Cli.State (State)
import Cli.State.NwWraper (unwrapN, withNetwork) as State
import Cli.Components.Link as Link
import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.FullInfoBox as FI

import Noodle.Id as Id
import Noodle.Network as Network
import Noodle.Node (Node)
import Noodle.Node (unsafeDisconnect) as Node
import Noodle.Patch (removeNode, allLinksOf, withLink) as Patch
import Noodle.Patch.Has as PHas

import Toolkit.Hydra (Instances) as Hydra
import Toolkit.Hydra.Repr.Wrap (WrapRepr) as H


component
    :: forall f instances' state is os
     . IsSymbol f
    => PHas.HasInstancesOf f instances' (Hydra.Instances Effect) (Array (Node f state is os H.WrapRepr Effect))
    => Offset
    -> Id.Family f
    -> Node f state is os H.WrapRepr Effect
    -> NodeBoxKey
    -> InfoBoxKey
    -> RemoveButtonKey
    -> Core.Blessed State
component topOffset family node nodeBoxKey infoBoxKey buttonKey =
    B.button buttonKey
        [ Box.content $ T.singleLine $ T.removeButtonOut
        , Box.top topOffset
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 3
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inputsOutputs
        , Core.on Button.Press
            \_ _ -> do
                state <- State.get
                case Tuple.snd <$> state.currentPatch of
                    Just patchId ->
                        case Map.lookup patchId state.patchKeysMap of
                            Just patchBoxKey -> do
                                case Network.patch patchId $ State.unwrapN state.network of
                                    Just patch ->
                                        for_ (Patch.allLinksOf node patch) \holdsLink -> Patch.withLink holdsLink Node.unsafeDisconnect
                                    Nothing -> pure unit
                                State.modify_
                                    (_ { network = State.withNetwork (Network.withPatch patchId $ Patch.removeNode node) state.network })
                                Link.removeAllOf nodeBoxKey patchBoxKey
                                State.modify_ $ Link.forgetAllFromTo nodeBoxKey
                                nodeBoxKey >~ Node.detach
                                Key.mainScreen >~ Screen.render
                            Nothing -> pure unit
                    Nothing -> pure unit
        , Core.on Element.MouseOver
            \_ _ -> do
                buttonKey >~ Box.setContent $ T.singleLine $ T.removeButtonOver
                infoBoxKey >~ IB.removeInfo
                SL.removeStatus family
                FI.removeStatus family
                Key.mainScreen >~ Screen.render -- FIXME: refresh only the area
        , Core.on Element.MouseOut
            \_ _ -> do
                buttonKey >~ Box.setContent $ T.singleLine $ T.removeButtonOut
                -- Info box : delete this node
                infoBoxKey >~ IB.clear
                SL.clear
                FI.clear
                Key.mainScreen >~ Screen.render -- FIXME: refresh only the area
        ]
        []