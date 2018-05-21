module Rpd.Render.Create
    ( createRenderer
    ) where

import Prelude
import Rpd.Render

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (head, tail, (:))
import Data.Array as Array
import Data.Filterable (filter)
import Data.Map (Map, mapWithKey)
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


-- https://dvdsgl.co/2016/a-trello-monad-in-the-dark/

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
        dataFlow = foldH dataFoldingF' uiMsgFlow $ pure (Map.empty /\ Map.empty)
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
    -> R.RpdEff e (R.Cancelers e)
    -> R.RpdEff e (R.Cancelers e)
dataFoldingF
    inletHandler
    outletHandler
    ((UI _ network) /\ msg)
    cancellersEff = do
    ( allOutletCancelers /\ allInletCancelers ) :: R.Cancelers e <- cancellersEff
    case msg of
        {- AddNode -> pure cancelers -- FIXME: implement -}
        SubscribeAllData ->
            let
                ( allOutletSubscribers /\ allInletSubscribers ) = subscribeAll network
                allOutletCancelers' :: Map R.OutletPath (R.Canceler e)
                allOutletCancelers' =
                    map performSub allOutletSubscribers
                allInletCancelers' :: Map R.InletPath (Array (R.Canceler e))
                allInletCancelers' =
                    map (map performSub) allInletSubscribers
            in pure $ allOutletCancelers' /\ allInletCancelers'
            {- subscribe with function below and execute all -}
        -- FIXME: implement
        ConnectTo inlet -> do
                let
                    maybeCanceler :: Maybe (R.Canceler e)
                    maybeCanceler = connectToInlet inlet network <#> performSub
                case maybeCanceler of
                    Just canceler ->
                        let
                            inletCancelers' :: Array (R.Canceler e)
                            inletCancelers' =
                                case Map.lookup inlet allInletCancelers of
                                    Just inletCancelers -> canceler : inletCancelers
                                    Nothing -> [ canceler ]
                            allInletCancelers' :: Map R.InletPath (Array (R.Canceler e))
                            allInletCancelers' =
                                Map.insert inlet inletCancelers' allInletCancelers
                        in pure $ allOutletCancelers /\ allInletCancelers'
                    Nothing -> pure $ allOutletCancelers /\ allInletCancelers
            {- subscribe with function below and execute subscriber,
            -- then insert the resulting canceler into the map -}
            -- connectToInlet inlet network
        -- FIXME: implement
        DisconnectAt inlet -> do
            -- _ <- fromMaybe (pure unit) $ disconnectAtInlet inlet allInletCancelers <#> performCancel
            case disconnectAtInlet inlet allInletCancelers <#> performCancel of
                Just eff -> do
                    _ <- eff
                    let
                        inletCancelers' :: Array (R.Canceler e)
                        inletCancelers' =
                            -- FIMXE: the search is performed second time, first time at disconnectAtInlet
                            case Map.lookup inlet allInletCancelers of
                                Just inletCancelers -> fromMaybe [] $ tail inletCancelers -- tail inletCancelers <|> []
                                Nothing -> [ ]
                        allInletCancelers' :: Map R.InletPath (Array (R.Canceler e))
                        allInletCancelers' =
                            Map.insert inlet inletCancelers' allInletCancelers
                    pure $ allOutletCancelers /\ allInletCancelers'
                Nothing -> pure $ allOutletCancelers /\ allInletCancelers
            {- execute the canceler returned from function below,
            -- then remove it from the map -}
        _ -> pure ( allOutletCancelers /\ allInletCancelers )
    where
        performSub :: R.Subscriber e -> R.Canceler e
        performSub sub = do
            canceler :: R.Canceler e <- sub
            c <- canceler
            pure c
        performCancel :: R.Canceler e -> R.RpdEff e Unit
        performCancel can = do
            _ <- can
            pure unit
        subscribeAll :: R.Network d -> R.Subscribers e
        subscribeAll network =
            R.subscribeAll
                (\inlet _ d -> inletHandler d inlet)
                (\outlet d -> outletHandler d outlet)
                network
        connectToInlet :: R.InletPath -> R.Network d -> Maybe (R.Subscriber e)
        connectToInlet inlet network =
            R.subscribeTop (\_ d -> inletHandler d inlet) inlet network
        disconnectAtInlet :: R.InletPath -> Map R.InletPath (Array (R.Canceler e)) -> Maybe (R.Canceler e)
        disconnectAtInlet inlet allInletCancelers =
            Map.lookup inlet allInletCancelers >>= head


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
