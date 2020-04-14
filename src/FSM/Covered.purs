module FSM.Covered
    ( CoveredFSM
    , fine, fineDo
    , follow, followJoin
    , foldUpdate
    ) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))
import Data.Covered (Covered(..), carry, appendError, cover, uncover', recover, mapError, joinErrors)

import Effect (Effect)
import Effect.Ref as Ref

import FRP.Event (Event)
import FRP.Event as Event

import FSM
import FSM (foldUpdate) as FSM


type CoveredFSM error action model =
    FSM action (Covered error model)


-- TODO: try `Semigroup error`


fine :: forall error action model. DoNothing action => model -> Covered error model /\ Effect action
fine nw = pure nw /\ pure doNothing


fineDo
    :: forall error action model
     . model
    -> Effect action
    -> Covered error model /\ Effect action
fineDo nw eff = pure nw /\ eff
-- TODO: make an operator out of it


-- is it some combination of bind and mapping?
-- since it's `m a -> (a -> m a) -> m a`
follow
    :: forall error action model
     . Batch action
    => Covered error model /\ Effect action
    -> (model -> Covered error model /\ Effect action)
    -> Covered error model /\ Effect action
follow (Recovered err v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <:> x')
        Carried v' /\ x' -> Recovered err v' /\ (x <:> x')
follow (Carried v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <:> x')
        Carried v' /\ x' -> Carried v' /\ (x <:> x')


followJoin
    :: forall error action model
     . Batch action
    => Semigroup error
    => Covered error model /\ Effect action
    -> (model -> Covered error model /\ Effect action)
    -> Covered error model /\ Effect action
followJoin (Recovered err v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered (err <> err') v' /\ (x <:> x')
        Carried v' /\ x' -> Recovered err v' /\ (x <:> x')
followJoin (Carried v /\ x) f =
    case f v of
        Recovered err' v' /\ x' -> Recovered err' v' /\ (x <:> x')
        Carried v' /\ x' -> Carried v' /\ (x <:> x')


{- FIMXE: this should be somehow unified with FSM.foldUpdate or even integrated into the FSM type -}
{-        implement `Foldable`? -}
foldUpdate
    :: forall error action model
     . Semigroup error
    => Batch action
    => (action -> Covered error model ->  Covered error model /\ Effect action)
    -- => CoveredFSM error action model
    -> model
    -> ( action /\ action )
    -> Covered error model /\ Effect action
foldUpdate updateF model ( actionA /\ actionB ) =
    FSM.foldUpdate
        (\action model' ->
            let model'' /\ effects = updateF action model'
            in joinErrors model' model'' /\ effects
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
