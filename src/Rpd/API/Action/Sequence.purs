module Rpd.API.Action.Sequence where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Ref as Ref
import Effect.Console (log) as Console

import Control.Monad.Error.Class
import Control.Alternative ((<|>))

import Data.Array ((:))
import Data.Array (snoc) as Array
import Data.List (List(..), snoc)
import Data.Either
import Data.Maybe
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (sequence, traverse_)

import FRP.Event (Event)
import FRP.Event as Event

import Data.Covered (Covered)
import Data.Covered (fromEither, fromEither', carry, appendError, recover, cover) as Covered

import FSM.Covered (CoveredFSM)
import FSM (make) as FSM

import Rpd.Network
import Rpd.API
    ( NodeInletsSubscription, NodeOutletsSubscription
    , InletSubscription, OutletSubscription
    )
import Rpd.API.Errors (RpdError)
import Rpd.API.Action
import Rpd.API.Action.Apply (Step, apply)
import Rpd.Path as Path
import Rpd.Toolkit (Toolkit, NodeDef)
import Rpd.Util (Canceler)


type ActionList d c n = List (Action d c n) -- TODO: or newtype?
-- type EveryStep d c n = Either RpdError (Network d c n) -> Effect Unit
-- type EveryAction d c n = Action d c n -> Effect Unit
-- type FoldResult d c n = Either RpdError (Network d c n)
-- type BasicContinuationResult model action =
--     { models :: Event (Either RpdError model)
--     , actions :: Event action
--     , pushAction :: action -> Effect Unit
--     , stop :: Canceler
--     }
-- type ContinuationResult d c n =
--     BasicContinuationResult (Network d c n) (Action d c n)


infixl 1 andThen as </>


init :: forall d c n. ActionList d c n
init = Nil


addPatch :: forall d c n. Path.Alias -> Action d c n
addPatch = Request <<< ToAddPatch


addNode :: forall d c n. Path.ToPatch -> Path.Alias -> n -> Action d c n
addNode patch alias n = Request $ ToAddNode patch alias n


addNextNode :: forall d c n. Path.ToPatch -> n -> Action d c n
addNextNode patch n = Request $ ToAddNextNode patch n


addNodeByDef :: forall d c n. Path.ToPatch -> Path.Alias -> n -> NodeDef d c -> Action d c n
addNodeByDef patch alias n def = Request $ ToAddNodeByDef patch alias n def


addNextNodeByDef :: forall d c n. Path.ToPatch -> n -> NodeDef d c -> Action d c n
addNextNodeByDef patch n def = Request $ ToAddNextNodeByDef patch n def


removeNode :: forall d c n. Path.ToNode -> Action d c n
removeNode = Request <<< ToRemoveNode


addInlet :: forall d c n. Path.ToNode -> Path.Alias -> c -> Action d c n
addInlet node alias c = Request $ ToAddInlet node alias c


addOutlet :: forall d c n. Path.ToNode -> Path.Alias -> c -> Action d c n
addOutlet node alias c = Request $ ToAddOutlet node alias c


removeInlet :: forall d c n. Path.ToInlet -> Action d c n
removeInlet inlet = Request $ ToRemoveInlet inlet


removeOutlet :: forall d c n. Path.ToOutlet -> Action d c n
removeOutlet outlet = Request $ ToRemoveOutlet outlet


connect :: forall d c n. Path.ToOutlet -> Path.ToInlet -> Action d c n
connect outlet inlet = Request $ ToConnect outlet inlet


disconnect :: forall d c n. Path.ToOutlet -> Path.ToInlet -> Action d c n
disconnect outlet inlet = Request $ ToDisconnect outlet inlet


sendToInlet :: forall d c n. Path.ToInlet -> d -> Action d c n
sendToInlet inlet d = Request $ ToSendToInlet inlet d


sendToOutlet :: forall d c n. Path.ToOutlet -> d -> Action d c n
sendToOutlet outlet d = Request $ ToSendToOutlet outlet d


streamToInlet :: forall d c n. Path.ToInlet -> (Event d) -> Action d c n
streamToInlet inlet event = Request $ ToStreamToInlet inlet event


streamToOutlet :: forall d c n. Path.ToOutlet -> (Event d) -> Action d c n
streamToOutlet outlet event = Request $ ToStreamToOutlet outlet event


subscribeToInlet :: forall d c n. Path.ToInlet -> InletSubscription d -> Action d c n
subscribeToInlet inlet handler = Request $ ToSubscribeToInlet inlet handler


subscribeToOutlet :: forall d c n. Path.ToOutlet -> OutletSubscription d -> Action d c n
subscribeToOutlet outlet handler = Request $ ToSubscribeToOutlet outlet handler


subscribeToNode
    :: forall d c n
     . Path.ToNode
    -> NodeInletsSubscription d
    -> NodeOutletsSubscription d
    -> Action d c n
subscribeToNode node inletsHandler outletsHandler =
    Request $ ToSubscribeToNode node inletsHandler outletsHandler


do_ :: forall d c n. (Perform d c n) -> Action d c n
do_ f = Inner $ Do f


-- pass :: forall d c n. EveryStep d c n
pass :: forall d c n. Either RpdError (Network d c n) -> Effect Unit
pass = const $ pure unit


type Sequence d c n = CoveredFSM RpdError (Action d c n) (Network d c n)


make :: forall d c n. Toolkit d c n -> Sequence d c n
make toolkit =
    FSM.make
        $ \action coveredModel ->
            ((<$>) sequence) <$> apply toolkit action $ Covered.recover coveredModel
            {-
            let
                (nextCoveredModel /\ rpdEffects) =
                    -- TODO: make apply return `Covered`.
                    -- also effects should not be under error control
                    case apply toolkit action recovered of
                        Left err -> Covered.cover recovered err /\ []
                        Right (nextModel /\ effects) ->
                            Covered.carry nextModel /\ effects
                    where recovered = Covered.recover coveredModel
            in
                nextCoveredModel /\ [] -- FIXME: convert RpdEffect to `Effect (Action d c n)`
            -}


andThen :: forall d c n. ActionList d c n -> Action d c n -> ActionList d c n
andThen = snoc


pushAll :: forall d c n. (Action d c n -> Effect Unit) -> ActionList d c n -> Effect Unit
pushAll = traverse_
