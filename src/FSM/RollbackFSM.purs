module FSM.Rollback
    ( RollbackFSM
    , make, make'
    , makeWithPush, makeWithPush'
    , run, run', run''
    , fold
    , followJoin -- FIXME: do not expose
    ) where


import Prelude


import Effect (Effect)

import Control.Alt ((<|>))

import Data.Tuple.Nested ((/\), type (/\))
import Data.Covered (Covered(..),  appendErrors)

import FSM (FSM, AndThen, (<:>))
import FSM (make, makeWithPush, joinWith) as FSM
import FSM (run, run', run'', fold) as FSM


newtype RollbackFSM error action model =
    RollbackFSM (FSM action (Covered error model))


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


makeWithPush
    :: forall error action model
     . (
               (action -> Effect Unit)
            -> action
            -> Covered error model
            -> Covered error model /\ Effect (AndThen action)
       )
    -> RollbackFSM error action model
makeWithPush = FSM.makeWithPush >>> toRollback


makeWithPush'
    :: forall error action model
     . Semigroup error
    => (
               (action -> Effect Unit)
            -> action
            -> Covered error model
            -> Covered error model /\ Effect (AndThen action)
       )
    -> RollbackFSM error action model
makeWithPush' = FSM.makeWithPush >>> toRollback'


run (RollbackFSM fsm) = FSM.run fsm


run' (RollbackFSM fsm) = FSM.run' fsm


run'' (RollbackFSM fsm) = FSM.run'' fsm


fold (RollbackFSM fsm) = FSM.fold fsm



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


-- mapError
--     :: forall error action model
--      . Semigroup error
--     => RollbackFSM error actionA modelA
--     -> RollbackFSM error (Either actionA actionB) (modelA /\ modelB)
--     -> RollbackFSM (Either actionA actionB) (modelA /\ modelB)
-- mapError =
