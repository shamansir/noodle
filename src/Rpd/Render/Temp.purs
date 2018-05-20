
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
   ( allOutletCancelers /\ allInletCancelers ) <- cancellersEff
    let
        subscribers :: R.Subscribers e
        subscribers =
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
                        maybeSubscriber :: Maybe (R.Subscriber e)
                        maybeSubscriber =
                            R.subscribeTop (\_ d -> inletHandler d inlet) inlet network
                    in do
                        case maybeSubscriber of
                            Just subscriber -> do
                                canceler <- subscriber
                                let
                                    -- canceler :: R.Canceler e
                                    -- canceler = do
                                    --     c <- subscriber
                                    --     c
                                    inletCancelers' :: Array (R.Canceler e)
                                    inletCancelers' =
                                        case Map.lookup inlet allInletCancelers of
                                            Just inletCancelers -> canceler : inletCancelers
                                            Nothing -> [ canceler ]
                                    allInletCancelers' :: Map R.InletPath (R.Canceler e)
                                    allInletCancelers' =
                                        -- Map.insert inlet inletCancelers' allInletCancelers
                                        ?what
                                pure $ allOutletCancelers /\ allInletCancelers'
                            Nothing -> pure $ allOutletCancelers /\ allInletCancelers
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
    pure subscribers
