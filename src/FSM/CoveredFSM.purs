module FSM.Covered
    ( CoveredFSM
    , fine, fineDo
    , follow, followJoin
    ) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))
import Data.Covered (Covered(..), carry, appendError, cover, uncover', recover, mapError)

import Effect (Effect)
import Effect.Ref as Ref

import FRP.Event (Event)
import FRP.Event as Event

import FSM


type CoveredFSM error action model = FSM action (Covered error model)


fine :: forall error action model. model -> Covered error model /\ Array (Effect action)
fine nw = pure nw /\ []


fineDo
    :: forall error action model
     . model
    -> Effect action
    -> Covered error model /\ Array (Effect action)
fineDo nw eff = pure nw /\ [ eff ]
-- TODO: make an operator out of it


-- is it some combination of bind and mapping?
-- since it's `m a -> (a -> m a) -> m a`
follow
    :: forall a e x
     . Semigroup x
    => Covered e a /\ x
    -> (a -> Covered e a /\ x)
    -> Covered e a /\ x
follow (Recovered err v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <> x')
        Carried v' /\ x' -> Recovered err v' /\ (x <> x')
follow (Carried v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <> x')
        Carried v' /\ x' -> Carried v' /\ (x <> x')


followJoin
    :: forall a e x
     . Semigroup x
    => Semigroup e
    => Covered e a /\ x
    -> (a -> Covered e a /\ x)
    -> Covered e a /\ x
followJoin (Recovered err v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered (err <> err') v' /\ (x <> x')
        Carried v' /\ x' -> Recovered err v' /\ (x <> x')
followJoin (Carried v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <> x')
        Carried v' /\ x' -> Carried v' /\ (x <> x')


{-
fold
    :: forall error action model
     . CoveredFSM error action model
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
