module Rpd.Render.MUV'
    ( Renderer(..)
    , UpdateF
    , ViewF
    , once
    , run
    , run'
    , make
    , make'
    ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldr)

import Effect (Effect)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd (run) as R
import Rpd.API ((</>))
import Rpd.API (Rpd, RpdError) as R
import Rpd.API as Rpd
import Rpd.Path (toNode, ToNode(..), ToOutlet(..)) as R
import Rpd.Command as C
import Rpd.CommandApply as C
import Rpd.Network (Network) as R
import Rpd.Util (Canceler) as R
import Rpd.Render as R


-- "Action" is either core "command" or user "message"
-- the naming is too confusing though
-- type Action msg d = Either msg (C.Command d)
    -- = Core (C.Command d)
    -- | User msg


{- UpdateF:
   - gets message: either core one from Rpd.Render, or the custom one used by user in the MUV loop;
   - gets the latest MUV model paired with the latest network state;
   - and returns new MUV model with an array of messages to execute in the next loop, when needed;

   TODO: let user do effects in `UpdateF` or consider returning messages as providing the way to return such effects.
-}
type UpdateF d model
    = C.Command d
    -> model /\ R.Network d
    -> model /\ Array (C.Command d)
{- ViewF:
   - gets the function allowing to push messages to the flow (for use in view handlers);
   - gets the latest MUV model paired with the latest network state;
   - and returns new view built using these states;
-}
type ViewF d model view =
    R.PushCmd d -> Either R.RpdError (model /\ R.Network d) -> view


data Renderer d model view
    = Renderer
        { from :: view -- initial view
        , init :: model -- initial state
        , update :: UpdateF d model
        , view :: ViewF d model view
        -- , mapMessage :: C.Command d -> msg -- maps core command to the user messages if user want to handle both / TODO: get rid of
        -- , mapCommand :: msg -> C.Command d
        }


-- core :: forall d m. Core.Message d -> Message d m
-- core = Core


-- custom :: forall d m. m -> Message d m
-- custom = Custom


extractRpd
    :: forall d model view
     . ViewF d model view
    -> R.PushCmd d
    -> R.Rpd (model /\ R.Network d)
    -> Effect view
extractRpd view push rpd =
    R.run onError onSuccess rpd
    where
        onError err = view push $ Left err
        onSuccess res = view push $ Right res


{- render once -}
once
    :: forall d model view
     . Renderer d model view
    -> R.Rpd (R.Network d)
    -> Effect view
once (Renderer { view, init, update }) rpd =
    extractRpd view R.neverPush withModel
    where
        withModel = (/\) init <$> rpd


{- Prepare the rendering cycle with internal message producer.
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe` returns the
   canceler, so it is possible to stop the thing.
-}
make
    :: forall d model view
     . R.Rpd (R.Network d)
    -> Renderer d model view
    -> Effect
        { first :: view
        , next :: Event (Effect view)
        }
make nw renderer =
    Event.create >>=
        \event -> pure $ make' event nw renderer


{- Prepare the rendering cycle with custom message producer
   (so, the `Event` with the messages source and
   the function which pushes them to this flow).
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe` returns the
   canceler, so it is possible to stop the thing.

   TODO: do not ask user for `event`, just pushing function.
-}
make'
    :: forall d model view
     . { event :: Event (C.Command d)
       , push :: C.Command d -> Effect Unit
       }
    -> R.Rpd (R.Network d)
    -> Renderer d model view
    -> { first :: view
       , next :: Event (Effect view)
       }
make'
    { event, push }
    rpd
    (Renderer { from, init, view, update })
    = let
        updateFlow = Event.fold updatePipeline event $ (/\) init <$> rpd
        viewFlow = viewer <$> updateFlow
    in
        { first : from
        , next : viewFlow
        }
    where
        -- C.apply
        -- Event.folded :: forall event a. IsEvent event => Monoid a => event a -> event a
        pushCommand
            = R.neverPush
        -- update :: msg -> (model /\ R.Network d) -> PushMsg msg -> R.PushCmd d -> R.Rpd (model /\ R.Network d)
        updatePipeline
            :: C.Command d
            -> R.Rpd (model /\ R.Network d)
            -> R.Rpd (model /\ R.Network d)
        updatePipeline cmd rpd = rpd >>=
            \(model /\ nw) -> do
                nw' <- C.apply cmd push nw
                -- perform the user update function with this core command, collect the returned messages
                let model' /\ actions = update cmd $ model /\ nw'
                -- apply user messages returned from previous line to the model
                foldr updatePipeline (pure $ model' /\ nw') actions
        viewer
            :: R.Rpd (model /\ R.Network d)
            -> Effect view
        viewer =
            extractRpd view (R.PushCmd push)


{- Run the rendering cycle without any special handling
   (so the rendering results are omitted).

   Returns the canceler. -}
run
    :: forall d view model
     . R.Rpd (R.Network d)
    -> Renderer d view model
    -> Effect R.Canceler
run nw renderer =
    make nw renderer >>=
        \{ first, next } -> Event.subscribe next (pure <<< identity)


{- Run the rendering cycle with custom message producer
   (so, the `Event` with the messages source and
   the function which pushes them to this flow).
   Returns the first view and the event flow with
   all the next views.

   TODO: do not ask user for `event`, just pushing function.
-}
run'
    :: forall d view model
     . { event :: Event (C.Command d)
       , push :: C.Command d -> Effect Unit
       }
    -> R.Rpd (R.Network d)
    -> Renderer d view model
    -> Effect R.Canceler
run' event nw renderer =
    case make' event nw renderer of
        { first, next } -> Event.subscribe next (pure <<< identity)

