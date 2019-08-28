module Rpd.Render.MUV
    ( Renderer(..)
    , PushF(..)
    , ViewF
    , UpdateF
    , PerformEffectF
    , make
    , once
    , skipEffects
    , fromMinimal
    ) where


import Prelude
import Effect

import Data.Either (Either(..), either)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))

import FRP.Event (Event)
import FRP.Event as Event

import Rpd.Network as R
import Rpd.API as R
import Rpd.API.Action as Core
import Rpd.API.Action.Apply as Core
import Rpd.API.Action.Sequence (prepare_) as ActionSeq
import Rpd.Toolkit as T
import Rpd.Render.Minimal as Minimal


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
 -- FIXME: toolkit renderer should be present among the arguments
type ViewF d c n model view action
     = Either R.RpdError (model /\ R.Network d c n)
    -> view


-- TODO: rename `effect` to `perform` (so users could tend to name it `Perform`/`Do`)
-- and `performEffect` to just `perform`?
type PerformEffectF d c n model action effect
     = T.Toolkit d c n
    -> (Either action (Core.Action d c n) -> Effect Unit)
    -> effect
    -> (model /\ R.Network d c n)
    -> Effect Unit


data Renderer d c n model view action effect
    = Renderer
        { from :: view -- initial view
        , init :: model -- initial state
        , update :: UpdateF d c n model action effect
        , view :: ViewF d c n model view action
        , performEffect :: PerformEffectF d c n model action effect
        }


skipEffects :: forall d c n model action effect. PerformEffectF d c n model action effect
skipEffects = const $ const $ const $ const $ pure unit


noUpdates :: forall d c n model action effect. model -> UpdateF d c n model action effect
noUpdates defaultModel = const $ const $ const $ defaultModel /\ []


neverPush :: forall d c n action. PushF d c n action
neverPush = PushF $ const $ pure unit


make
    :: forall d c n model view action effect
     . Renderer d c n model view action effect
    -> T.Toolkit d c n
    -> R.Network d c n
    -> Effect
        { first :: view
        , next :: Event view
        , push :: PushF d c n action
        , stop :: Effect Unit
        }
make (Renderer { from, init, update, view, performEffect }) toolkit initialNW = do
    { models, pushAction, stop } <-
        ActionSeq.prepare_
            (init /\ initialNW)
            myApply
            myPerformEff
    { event : views, push : pushView } <- Event.create
    stopViews <- Event.subscribe models $ pushView <<< view
    pure
        { first : from
        , next : views
        , push : PushF pushAction
        , stop : stop <> stopViews
        }
    where
        myApply (Right coreAction) (model /\ nw) = do
            nw' /\ coreEffects <- Core.apply toolkit coreAction nw
            let model' /\ userEffects = update toolkit (Right coreAction) $ model /\ nw'
            let allEffects = (Right <$> coreEffects) <> (Left <$> userEffects)
            pure $ (model' /\ nw') /\ allEffects
        myApply (Left userAction) (model /\ nw) = do
            let model' /\ userEffects = update toolkit (Left userAction) $ model /\ nw
            pure $ (model' /\ nw) /\ (Left <$> userEffects)
        myPerformEff pushAction (Right coreEffect) (_ /\ nw) =
            Core.performEffect toolkit (pushAction <<< Right) coreEffect nw
        myPerformEff pushAction (Left userEffect) (model /\ nw) =
            performEffect toolkit pushAction userEffect (model /\ nw)


once
    :: forall d c n model view action effect
     . Renderer d c n model view action effect
    -> T.Toolkit d c n
    -> R.Network d c n
    -> view
once (Renderer { init, view }) _ nw =
    view $ Right $ init /\ nw



fromMinimal
    :: forall d c n view
     . Minimal.Renderer d c n view
    -> Renderer d c n Unit view Unit Unit
fromMinimal (Minimal.Renderer minimal) =
    Renderer
        { from : minimal.first -- initial view
        , init : unit -- initial state
        , update : noUpdates unit
        , view : minimalView
        , performEffect : skipEffects
        }
    where
        minimalView valueE =
            either
                minimal.viewError
                (minimal.viewValue <<< snd)
                valueE


-- data MyData = A | B
-- data MyChannel = P | Q
-- data MyNode = X | Z

-- type MyModel = { v :: Boolean /\ String }
-- type MyView = String
-- data MyAction = T | U
-- data MyEffect = S | Y


-- sampleRenderer ::
--     Renderer MyData MyChannel MyNode MyModel MyView MyAction MyEffect
-- sampleRenderer =
--     Renderer
--         { from : ""
--         , init : { v : true /\ "AAA" }
--         , update : \_ _ _ -> { v : true /\ "BBB" } /\ []
--         , view : \_ _ -> "VIEW"
--         , performEffect : \_ _ _ _ -> pure unit
--         }
