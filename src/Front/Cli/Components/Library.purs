module Cli.Components.Library where


import Control.Monad.State as State

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (liftEffect)

import Effect.Console as Console

import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))
import Data.Array ((!!))

import Data.Text.Format (fgc, s) as T
import Data.Text.Output.Blessed (singleLine) as T

import Data.Argonaut.Decode (decodeJson)

import Blessed as B
import Blessed ((>~), (~<))

import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Border as Border
import Blessed.Core.ListStyle as LStyle
import Blessed.Core.EndStyle as ES

import Blessed.Internal.Core as Core
import Blessed.Internal.NodeKey as NodeKey
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Lists.List.Event (ListEvent(..)) as List
import Blessed.UI.Lists.List.Option (items, keys, mouse, style) as List
import Blessed.UI.Lists.List.Property (selected) as List

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style (library, libraryBorder) as Style

import Noodle.Id (FamilyR) as Id
import Noodle.Repr (class HasFallback)
import Noodle.Network as Network
import Noodle.Toolkit as Toolkit
import Noodle.Toolkit (Toolkit)
import Noodle.Ui.Cli.Tagging (libraryItem) as T
import Noodle.Wiring (class Wiring)
import Noodle.Fn.ToFn (class PossiblyToFn)

import Cli.Components.NodeBox as NodeBox
import Cli.Class.CliFriendly (class CliFriendly)


import Prelude


component
    :: forall tk p fs repr
     . Toolkit.HoldsFamilies repr Effect fs
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => CliFriendly tk fs repr Effect
    => HasFallback repr
    => Toolkit tk fs repr Effect
    -> Core.Blessed (State tk p fs repr Effect) -- TODO: the only thing that makes it require `Effect` is `Core.on List.Select` handler, may be there's a way to overcome it ...
    -- -> BlessedOpM (State tk p fs repr m) m Unit
component toolkit =
    B.listAnd Key.library
        [ Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.px 20
        , Box.height $ Dimension.percents 65.0
        , Box.draggable true
        , Box.scrollable true
        , List.items $ (T.singleLine <<< T.libraryItem (Proxy :: _ tk)) <$> Toolkit.families toolkit
        , List.mouse true
        , List.keys true
        , Box.tags true
        , Style.library
        , Style.libraryBorder
        {- REM
        , Core.on Element.MouseMove
            \_ evt -> do
                -- TODO show family info in status line & documentation
                selected <- List.selected ~< Key.library
                liftEffect $ Console.log $ show selected
        -}
        , Core.on List.Select
            \_ _ -> onFamilySelect
        ]
        []
        \_ ->
            pure unit



onFamilySelect
    :: forall tk pstate fs repr m
     . Wiring m
    => Toolkit.HoldsFamilies repr m fs
    => HasFallback repr
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => CliFriendly tk fs repr m
    => BlessedOpM (State tk pstate fs repr m) m Unit
onFamilySelect =
    do
        -- lastShiftX <- _.lastShiftX <$> State.get
        -- lastShiftY <- _.lastShiftY <$> State.get
        -- lastNodeBoxKey <- _.lastNodeBoxKey <$> State.get
        state <- State.get

        {- -}
        let mbCurrentPatchId = _.id <$> state.currentPatch
        let mbCurrentPatch = mbCurrentPatchId >>= \id -> Network.patch id state.network
        {- -}
        -- patchesBar >~ ListBar.addItemH ?wh [] ?wh

        -- Hydra.withFamily

        -- let top = Offset.px $ state.lastShiftX + 2
        -- let left = Offset.px $ 16 + state.lastShiftY + 2
        -- let nextNodeBox = NodeKey.next state.lastNodeBoxKey
        -- let nextInputsBox = NodeKey.next state.lastInputsBoxKey
        -- let nextOutputsBox = NodeKey.next state.lastOutputsBoxKey

        {- -}
        selected <- List.selected ~< Key.library
        let toolkit = Network.toolkit state.network
        let families = Toolkit.families toolkit
        let mbSelectedFamily = families !! selected
        let toolkit = Network.toolkit state.network

        {-
        let familyStr = fromMaybe "??" (Id.reflect' <$> mbSelectedFamily)

        Key.patchesBar >~ ListBar.setItems
            [ "test1" /\ [] /\ \_ _ -> do liftEffect $ Console.log "foo"
            , "test2" /\ [] /\ \_ _ -> do liftEffect $ Console.log "bar"
            , familyStr /\ [] /\ \_ _ -> do liftEffect $ Console.log familyStr
            ]
        -}

        -- mbNextNode <-
        _ <- case (/\) <$> mbSelectedFamily <*> ((/\) <$> mbCurrentPatch <*> mbCurrentPatchId) of
            Just (familyR /\ curPatch /\ curPatchId) ->
                let
                    createNodeBox rawFamily = NodeBox.fromRawFamilyAuto curPatch rawFamily toolkit
                in
                case Toolkit.withAnyFamily createNodeBox familyR toolkit of
                    Just op -> op
                    Nothing -> pure unit
            Nothing -> pure unit
        -- liftEffect $ Console.log $ show selected
        {- -}

        pure unit