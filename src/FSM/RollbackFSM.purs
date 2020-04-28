module FSM.Rollback
    ( RollbackFSM
    , fine, fineDo
    , follow, followJoin
    , foldUpdate
    ) where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Covered (Covered(..), carry, appendErrors)

import Effect (Effect)

import FSM (FSM, AndThen, doNothing, (<:>))
import FSM (foldUpdate) as FSM


type RollbackFSM error action model = FSM action (Covered error model) -- TODO: make a `newtype`


{-
toRollback
    :: forall error action model
     . FSM action (Covered error model)
    -> RollbackFSM error action model
toRollback fsm =
    RollbackFSM $ fsm # FSM.joinWith (<|>)


toRollback'
    :: forall error action model
     . Semigroup error
    => FSM action (Covered error model)
    -> RollbackFSM error action model
toRollback' fsm =
    RollbackFSM $ fsm # FSM.joinWith appendErrors


make
    :: forall error action model
     . (action -> Covered error model -> Covered error model /\ Effect (AndThen action))
    -> RollbackFSM error action model
make = FSM.make >>> toRollback


make'
    :: forall error action model
     . Semigroup error
    => (action -> Covered error model -> Covered error model /\ Effect (AndThen action))
    -> RollbackFSM error action model
make' = FSM.make >>> toRollback'
-}


-- TODO: try `Semigroup error`


fine :: forall error action model. model -> Covered error model /\ Effect (AndThen action)
fine model = pure model /\ doNothing


fineDo
    :: forall error action model
     . model
    -> Effect action
    -> Covered error model /\ Effect action
fineDo model eff = pure model /\ eff


-- TODO: try to get rid of those
-- is it some combination of bind and mapping?
-- since it's `m a -> (a -> m a) -> m a`
follow
    :: forall error action model
     . Covered error model /\ Effect (AndThen action)
    -> (model -> Covered error model /\ Effect (AndThen action))
    -> Covered error model /\ Effect (AndThen action)
follow (Recovered err v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <:> x')
        Carried v' /\ x' -> Recovered err v' /\ (x <:> x')
follow (Carried v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <:> x')
        Carried v' /\ x' -> Carried v' /\ (x <:> x')


-- TODO: try to get rid of those
followJoin
    :: forall error action model
     . Semigroup error
    => Covered error model /\ Effect (AndThen action)
    -> (model -> Covered error model /\ Effect (AndThen action))
    -> Covered error model /\ Effect (AndThen action)
followJoin (Recovered err v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered (err <> err') v' /\ (x <:> x')
        Carried v' /\ x' -> Recovered err v' /\ (x <:> x')
followJoin (Carried v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <:> x')
        Carried v' /\ x' -> Carried v' /\ (x <:> x')


-- TODO: try to get rid of those
foldUpdate
    :: forall error action model
     . Semigroup error
    => (action -> Covered error model -> Covered error model /\ Effect (AndThen action))
    -- => RollbackFSM error action model
    -> model
    -> ( action /\ action )
    -> Covered error model /\ Effect (AndThen action)
foldUpdate updateF model ( actionA /\ actionB ) =
    FSM.foldUpdate
        (\action model' ->
            let model'' /\ effects = updateF action model'
            in appendErrors model' model'' /\ effects
        )
        (carry model)
        ( actionA /\ actionB )
    -- let
    --     model' /\ effects' = updateF actionA $ carry model
    --     model'' /\ effects'' = updateF actionB model'
    -- in
    --     joinErrors model' model'' /\ (effects' <:> effects'')


{-
fold
    :: forall error action model
     . RollbackFSM error action model
    -> List action
    -> Effect
            ((List error /\ model) /\
            { pushAction :: action -> Effect Unit
            , stop :: Effect Unit
            })
fold fsm@(FSM initial _) actionList = do
    res@{ models, pushAction, stop } <- prepare fsm
    lastValRef <- Ref.new initialCovered
    let modelsFolded =
            Event.fold appendError models initialCovered
    stopCollectingLastValue <-
        Event.subscribe modelsFolded (flip Ref.write lastValRef)
    _ <- pushAll pushAction actionList
    lastVal <- Ref.read lastValRef
    pure $ uncover' lastVal /\ { pushAction, stop : stop <> stopCollectingLastValue }
    where initialCovered = mapError List.singleton initial
-}
