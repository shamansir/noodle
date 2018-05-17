module Rpd.Render.Create
    ( createRenderer
    ) where

import Prelude
import Rpd.Render

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (head, (:))
import Data.Array as Array
import Data.Filterable (filter)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Rpd as R
import Rpd.Flow
    ( Flow
    , create
    , subscribe
    , fold
    , sampleOn, sampleOn_
    , mapAccum
    , gateBy
    , withLast
    , foldH
    )


createRenderer :: forall d e. (Show d) => (Push d e -> UI d -> R.RenderEff e) -> R.Renderer d e
createRenderer render = (\nw -> do
    { flow : interactions, push : pushInteraction } <- create
    --{ flow : subs, push : pushSubEff } <- create
    let
        uiMsgFlow = fold foldingF interactions $ UI init nw /\ NoOp
        uiFlow = map fst uiMsgFlow
        dataFoldingF' =
            dataFoldingF
                (pushInletData pushInteraction)
                (pushOutletData pushInteraction)
        -- dataFoldingF'' :: (UI d /\ Message d) -> R.Cancelers e -> R.Cancelers e
        -- dataFoldingF'' = \uiMsg cancelers ->
        --     let
        --         eff :: R.RpdEff e (R.Cancelers e)
        --         eff = dataFoldingF' uiMsg cancelers
        --         -- cancelers' :: R.Cancelers e
        --         -- cancelers' = liftEff eff
        --         cancelers' :: R.Cancelers e
        --         cancelers' = do
        --             res <- pushSubEff eff
        --             r <- res
        --             pure r
        --     in cancelers'
        dataFlow = fold dataFoldingF' uiMsgFlow $ (Map.empty /\ Map.empty)
        -- dataFlow = withLast uiMsgFlow
    -- _ <- subscribe dataFlow $ \(ui /\ msg) _ -> log $ "aaa " <> show msg
    -- _ <- subscribe dataFlow (\(eff /\ msg /\ cancellers) -> do
        -- _ <- eff
        -- log $ "from subscriber: " <> show msg
    --)
    _ <- subscribe dataFlow (\_ -> pure unit) -- perform eff on the result
    _ <- subscribe uiMsgFlow $ \(ui /\ msg) -> do
        -- if messagAffectsSubscriptions msg
        --     then pure unit
        --     else pure unit
        render pushInteraction ui
    pushInteraction Init
    -- TODO: try `sampleOn`, may be it's the more proper thing to use
    --       instead of `fold` in case of data subscriptions/cancels.
    {- The code below should work instead, when
       https://github.com/paf31/purescript-behaviors/issues/27
       is dealt with. Like, folds start fresh on every subscription,
       and it is what breaks the flow.
    -}
    {-
    { flow : interactions, push : pushInteraction } <- create
    let
        uiMsgFlow = fold foldingF interactions $ UI init nw /\ NoOp
        uiFlow = map fst uiMsgFlow
        dataFoldingF' =
            dataFoldingF
                (pushInletData pushInteraction)
                (pushOutletData pushInteraction)
        dataFlow = fold dataFoldingF' uiMsgFlow $ pure (Map.empty /\ Map.empty)
    _ <- subscribe dataFlow id
    _ <- subscribe uiFlow $ \ui -> render pushInteraction ui
    pushInteraction Init
    -}
)

foldingF :: forall d. Interaction d -> (UI d /\ Message d) -> (UI d /\ Message d)
foldingF interaction (ui@(UI state _) /\ _) =
    updateAndLog msg ui /\ msg
    where msg = interactionToMessage interaction state


messagAffectsSubscriptions :: forall d. Message d -> Boolean
messagAffectsSubscriptions SubscribeAllData = true
messagAffectsSubscriptions (ConnectTo _) = true
messagAffectsSubscriptions (DisconnectAt _) = true
messagAffectsSubscriptions _ = false

dataFoldingF
    :: forall d e
     . (Show d)
    => (d -> R.InletPath -> R.RpdEff e Unit)
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
    -> (UI d /\ Message d)
    -> R.Cancelers e
    -> R.Cancelers e
dataFoldingF
    inletHandler
    outletHandler
    ((UI _ network) /\ msg)
    ( allOutletCancelers /\ allInletCancelers ) =
    let
        cancelers =
            {- pure $ -}
            case msg of
                -- AddNode -> pure cancelers -- FIXME: implement
                SubscribeAllData ->
                    -- TODO: subscribe to all inlets, outlets and their sources
                    -- subscriber <- subscribeData
                    --     (pushInletData pushInteraction)
                    --     (pushOutletData pushInteraction) network
                    R.subscribeAll
                        (\inlet _ d -> inletHandler d inlet)
                        (\outlet d -> outletHandler d outlet)
                        network
                ConnectTo inlet ->
                    let
                        canceler = R.subscribeTop (\_ d -> inletHandler d inlet) inlet network
                        allInletCancelers' = do
                            inletCancelers <- Map.lookup inlet allInletCancelers
                            canceler' <- canceler
                            let inletCancelers' = canceler' : inletCancelers
                                cancelers' = Map.insert inlet inletCancelers' allInletCancelers
                            pure cancelers'
                    in allOutletCancelers /\ fromMaybe allInletCancelers allInletCancelers'
                DisconnectAt inlet ->
                    -- TODO: think on the fact that last source could be not the found one!
                    -- (because user sources, etc.)
                    -- currently the logic of connecting/disconnecting + update, kinda guarantees that
                    -- it is the same one, however it's better to be sure and do not only trust the
                    -- core logic to be conformant with this one, but also may be introduce IDs to ensure
                    -- everything is properly arranged...
                    -- What to do with the Links in the Network also?
                    let
                        maybeCancel :: Maybe (R.Canceler e)
                        maybeCancel = Map.lookup inlet allInletCancelers >>= head
                        _ = case maybeCancel of
                            Just cancel -> do
                                log "perform cancel"
                                _ <- cancel
                                -- _ <- cancelEff
                                pure unit
                            Nothing -> pure unit
                    in
                        -- TODO: remove the canceller?
                        allOutletCancelers /\ allInletCancelers
                _ -> allOutletCancelers /\ allInletCancelers
        -- newCancelers :: R.Cancelers e
        -- newCancelers = do
        --     c <- subUnsubEff
        --     pure c
    in cancelers

pushInletData
    :: forall d e
     . (Interaction d -> R.RpdEff e Unit)
    -> (d -> R.InletPath -> R.RpdEff e Unit)
pushInletData push =
    (\d inletPath -> do
        -- log $ "Receive from " <> show inletPath
        push $ DataAtInlet inletPath d)


pushOutletData
    :: forall d e
     . (Interaction d -> R.RpdEff e Unit)
    -> (d -> R.OutletPath -> R.RpdEff e Unit)
pushOutletData push =
    (\d outletPath -> do
        --log $ "Receive from " <> show outletPath
        push $ DataAtOutlet outletPath d)



showCancelers :: forall e. R.Cancelers e -> String
showCancelers (outletCancelers /\ inletCancelers) =
    show $ "Outlets: " <> (show $ Map.keys outletCancelers) <>
           "Inlets: " <> (show $ Map.keys inletCancelers)
