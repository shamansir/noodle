module Cli.Components.Library where


import Control.Monad.State as State

import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

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
import Blessed.Internal.BlessedOp (lift') as Blessed

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Lists.List.Event (ListEvent(..)) as List
import Blessed.UI.Lists.List.Option (items, keys, mouse, style) as List
import Blessed.UI.Lists.List.Property (selected) as List

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style (library, libraryBorder) as Style

import Noodle.Id (PatchR, FamilyR, Family) as Id
import Noodle.Repr (class HasFallback, class FromRepr, class ToRepr)
import Noodle.Network as Network
import Noodle.Toolkit as Toolkit
import Noodle.Toolkit.Family (Family) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Toolkit (Toolkit)
import Noodle.Ui.Cli.Tagging (libraryItem) as T
import Noodle.Wiring (class Wiring)
import Noodle.Fn.ToFn (class PossiblyToFn)
import Noodle.Node (Node) as Noodle
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Toolkit.Family (Family) as Raw

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
        state <- State.get

        let mbCurrentPatchR = _.id <$> state.currentPatch
        selected <- List.selected ~< Key.library
        let toolkit = Network.toolkit state.network
        let families = Toolkit.families toolkit
        let mbSelectedFamily = families !! selected

        _ <- case (/\) <$> mbSelectedFamily <*> mbCurrentPatchR of
            Just (familyR /\ curPatchR) ->
                case Toolkit.withAnyFamily
                        (spawnAndRenderRaw
                            toolkit
                            curPatchR
                            familyR
                            $ NodeBox.nextPos state.lastShift)
                        familyR
                        toolkit
                    of
                    Just op -> op
                    Nothing -> pure unit
            Nothing -> pure unit

        pure unit


spawnAndRenderRaw
    :: forall  tk pstate fs repr m
     . Wiring m
    => Toolkit.HoldsFamilies repr m fs
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => CliFriendly tk fs repr m
    => HasFallback repr
    => Toolkit tk fs repr m
    -> Id.PatchR
    -> Id.FamilyR
    -> { left :: Int, top :: Int }
    -> Raw.Family repr repr m
    -> BlessedOpM (State tk pstate fs repr m) m Unit
spawnAndRenderRaw toolkit patchR familyR nextPos  _ = do
    (mbRawNode :: Maybe (Raw.Node repr repr m)) <- Blessed.lift' $ Toolkit.spawnAnyRaw familyR toolkit
    -- TODO: put node instance in the patch
    case mbRawNode of
        Just rawNode -> NodeBox.componentRaw nextPos patchR familyR rawNode
        Nothing -> pure unit


spawnAndRender
    :: forall tk fs pstate f nstate is os repr m
     . Wiring m
    => IsSymbol f
    => FromRepr repr nstate => ToRepr nstate repr
    => RegisteredFamily (F f nstate is os repr m) fs
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => CliFriendly tk fs repr m
    => Toolkit tk fs repr m
    -> Id.PatchR
    -> Id.Family f
    -> { left :: Int, top :: Int }
    -> Toolkit.Family f nstate is os repr m
    -> BlessedOpM (State tk pstate fs repr m) m _
spawnAndRender toolkit patchR family nextPos  _ = do
    (node :: Noodle.Node f nstate is os repr m) <- Blessed.lift' $ Toolkit.spawn family toolkit
    -- TODO: put node instance in the patch
    NodeBox.component nextPos patchR family node