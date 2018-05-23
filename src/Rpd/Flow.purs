module Rpd.Flow (test) where


type SubscriberEff e =
    RpdEff ( frp :: FRP | e) Unit
type Canceler e =
    RpdEff (frp :: FRP | e) Unit
type Subscriber e =
    RpdEff (frp :: FRP | e) (Canceler e)
type Cancelers e =
    Map OutletPath (Canceler e) /\ Map InletPath (Array (Canceler e))
type Subscribers e =
    Map OutletPath (Subscriber e) /\ Map InletPath (Array (Subscriber e))
-- newtype Canceler e =
--     Canceler (RpdEff (frp :: FRP | e) Unit)
-- newtype Subscriber e =
--     Subscriber (RpdEff (frp :: FRP | e) (RpdEff (frp :: FRP | e) Unit))
-- newtype Cancelers e =
--     Cancelers (Map OutletPath (Canceler e) /\ Map InletPath (Array (Canceler e)))
-- newtype Subscribers e =
--     Subscribers (Map OutletPath (Subscriber e) /\ Map InletPath (Array (Subscriber e)))

{- Data flow: subscriptions, cancelling, etc. -}

initCancelers :: forall e. Cancelers e
initCancelers = Map.empty /\ Map.empty


subscribeAll
    :: forall d e
     . (InletPath -> DataSource d -> d -> SubscriberEff e)
    -> (OutletPath -> d -> SubscriberEff e)
    -> Network d
    -> Subscribers e
subscribeAll inletHandler outletHandler (Network { patches }) =
    let
        allNodes = concatMap (\(Patch { nodes }) -> nodes) patches
        outletF = \o@(Outlet { path }) ->
                (/\) path <$> subscribeOutlet' (outletHandler path) o
        inletF = \i@(Inlet { path }) ->
                path /\ subscribeInlet' (inletHandler path) i
        -- allInletsAndOutlets = concatMap
        --     (\(Node { inlets, outlets }) -> inlets /\ outlets) allNodes
        allInlets = concatMap (\(Node { inlets }) -> inlets) allNodes
        allOutlets = concatMap (\(Node { outlets }) -> outlets) allNodes
    in (fromFoldable $ mapMaybe outletF allOutlets)
    /\ (fromFoldable $ map inletF allInlets)


subscribeNode
    :: forall d e
     . (InletPath -> DataSource d -> d -> SubscriberEff e)
    -> (OutletPath -> d -> SubscriberEff e)
    -> NodePath
    -> Network d
    -> Maybe (Subscribers e)
subscribeNode inletHandler outletHandler nodePath network =
    subscribeNode' inletHandler outletHandler <$> findNode nodePath network


subscribeNode'
    :: forall d e
     . (InletPath -> DataSource d -> d ->SubscriberEff e)
    -> (OutletPath -> d -> SubscriberEff e)
    -> Node d
    -> Subscribers e
subscribeNode' inletHandler outletHandler (Node { outlets, inlets }) =
    let
        outletF = \o@(Outlet { path }) ->
                (/\) path <$> subscribeOutlet' (outletHandler path) o
        inletF = \i@(Inlet { path }) ->
                path /\ subscribeInlet' (inletHandler path) i
    in (fromFoldable $ mapMaybe outletF outlets)
    /\ (fromFoldable $ map inletF inlets)


subscribeOutlet
    :: forall d e
     . (d -> SubscriberEff e)
    -> OutletPath
    -> Network d
    -> Maybe (Subscriber e)
subscribeOutlet f outletPath network =
    findOutlet outletPath network >>= subscribeOutlet' f


subscribeOutlet'
    :: forall d e
     . (d -> SubscriberEff e)
    -> Outlet d
    -> Maybe (Subscriber e) -- TODO: return subscriber to execute later instead
subscribeOutlet' f (Outlet { path, flow : maybeFlow }) =
    maybeFlow >>=
        \flow -> pure $ subscribe flow f


subscribeInlet
    :: forall d e
     . (DataSource d -> d -> SubscriberEff e)
    -> InletPath
    -> Network d
    -> Maybe (Array (Subscriber e))
subscribeInlet f inletPath network =
    subscribeInlet' f <$> findInlet inletPath network


subscribeInlet'
    :: forall d e
     . (DataSource d -> d -> SubscriberEff e)
    -> Inlet d
    -> Array (Subscriber e)
subscribeInlet' f (Inlet { path, sources }) =
    map
        (\source ->
            subscribe (getFlowOf source) (f source)
        ) sources


subscribeTop
    :: forall d e
     . (DataSource d -> d -> SubscriberEff e)
    -> InletPath
    -> Network d
    -> Maybe (Subscriber e)
subscribeTop f inletPath network =
    findInlet inletPath network >>= subscribeTop' f


subscribeTop'
    :: forall d e
     . (DataSource d -> d -> SubscriberEff e)
    -> Inlet d
    -> Maybe (Subscriber e)
subscribeTop' f (Inlet { sources }) =
    (\topSource ->
        subscribe (getFlowOf topSource) (f topSource)
    ) <$> head sources
