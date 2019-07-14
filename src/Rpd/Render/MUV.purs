module Rpd.Render.MUV
    ( Renderer(..)
    , PushF(..)
    , ViewF
    , UpdateF
    , PerformEffectF
    , make
    ) where


import Prelude
import Effect

import Data.Either (Either(..), either)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Network as R
import Rpd.API as R
import Rpd.API.Action as Core
import Rpd.API.Action.Apply as Core
import Rpd.Toolkit as T


data PushF d c n action  =
    PushF (Either action (Core.Action d c n) -> Effect Unit)


{- UpdateF:
   - gets message: either core one from Rpd.Render, or the custom one used by user in the MUV loop;
   - gets the latest MUV model paired with the latest network state;
   - and returns new MUV model with an array of messages to execute in the next loop, when needed;

   TODO: let user do effects in `UpdateF` or consider returning messages as providing the way to return such effects.
-}
type UpdateF d c n model action effect
     = T.Toolkit d c n
    -> Either action (Core.Action d c n)
    -> model /\ R.Network d c n
    -> model /\ Array effect

{- ViewF:
   - gets the function allowing to push messages to the flow (for use in view handlers);
   - gets the latest MUV model paired with the latest network state;
   - and returns new view built using these states;
-}
type ViewF d c n model view action
     = PushF d c n action
    -> Either R.RpdError (model /\ R.Network d c n)
    -> view


type PerformEffectF d c n model action effect
     = T.Toolkit d c n
    -> (action -> Effect Unit)
    -> effect
    -> model
    -> Effect Unit


data Renderer d c n model view action effect
    = Renderer
        { from :: view -- initial view
        , init :: model -- initial state
        , update :: UpdateF d c n model action effect
        , view :: ViewF d c n model view action
        , performEffect :: PerformEffectF d c n model action effect
        }


-- FIXME: in many things duplicates API.Action.Sequence.run
make
    :: forall d c n model view action effect
     . Renderer d c n model view action effect
    -> T.Toolkit d c n
    -> R.Network d c n
    -> Effect { first :: view, next :: Event view }
make (Renderer { from, init, update, view, performEffect }) toolkit initialNW = do
    { event : actions, push : pushAction } <- Event.create
    { event : views, push : pushView } <- Event.create
    let
        (updates
            :: Event
                (Either R.RpdError
                    { model :: model
                    , network :: R.Network d c n
                    , coreEffects :: Array (Core.RpdEffect d c n)
                    , userEffects :: Array effect
                    }
                )
        ) =
            Event.fold
                (\action step ->
                    case step of
                        Left err -> Left err
                        Right { model, network } -> do
                            network' /\ effects <-
                                case action of
                                    Right coreAction -> Core.apply toolkit coreAction network
                                    Left _ -> pure $ network /\ []
                            let model /\ userEffects = update toolkit action $ model /\ network'
                            pure
                                { model
                                , network : network'
                                , coreEffects : effects
                                , userEffects
                                }
                )
                actions
                (pure { model : init, network : initialNW, coreEffects : [], userEffects : []})
        (models :: Event (Either R.RpdError (model /\ R.Network d c n)))
            = ((<$>) \{ model, network } -> model /\ network) <$> updates
    _ <- Event.subscribe updates \step ->
        case step of
            Left err -> pure unit
            Right { model, network, coreEffects, userEffects } -> do
                coreEffects #
                    traverse_ (\eff ->
                        Core.performEffect toolkit (pushAction <<< Right) eff network
                    )
                userEffects #
                    traverse_ (\eff ->
                        performEffect toolkit (pushAction <<< Left) eff model
                    )
    _ <- Event.subscribe models (pushView <<< (view $ PushF pushAction))
    pure { first : from, next : views }


data MyData = A | B
data MyChannel = P | Q
data MyNode = X | Z

type MyModel = { v :: Boolean /\ String }
type MyView = String
data MyAction = T | U
data MyEffect = S | Y


sampleRenderer ::
    Renderer MyData MyChannel MyNode MyModel MyView MyAction MyEffect
sampleRenderer =
    Renderer
        { from : ""
        , init : { v : true /\ "AAA" }
        , update : \_ _ _ -> { v : true /\ "BBB" } /\ []
        , view : \_ _ -> "VIEW"
        , performEffect : \_ _ _ _ -> pure unit
        }
