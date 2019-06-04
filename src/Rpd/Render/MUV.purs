module Rpd.Render.MUV
    ( Renderer(..)
    , UpdateF
    , ViewF
    , PushF(..)
    , once
    , run
    , run'
    , make
    , make'
    ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (foldr)

import Effect (Effect)
import Effect.Class (liftEffect)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd (run) as R
import Rpd.API ((</>))
import Rpd.API (Rpd, RpdError) as R
import Rpd.API as Rpd
import Rpd.Path (toNode, ToInlet(..), ToOutlet(..)) as R
import Rpd.Command as C
import Rpd.CommandApply as C
import Rpd.Network (Network, Node, Inlet, Outlet) as R
import Rpd.Util (Canceler) as R
import Rpd.Render as R
import Rpd.Toolkit as T
-- import Rpd.Util (type (/->))

import Debug.Trace as DT


data PushF msg d = PushF (Either msg (C.Command d) -> Effect Unit)
{- UpdateF:
   - gets message: either core one from Rpd.Render, or the custom one used by user in the MUV loop;
   - gets the latest MUV model paired with the latest network state;
   - and returns new MUV model with an array of messages to execute in the next loop, when needed;

   TODO: let user do effects in `UpdateF` or consider returning messages as providing the way to return such effects.
-}
type UpdateF d model msg
    = Either msg (C.Command d)
    -> model /\ R.Network d
    -> model /\ Array (Either msg (C.Command d))
{- ViewF:
   - gets the function allowing to push messages to the flow (for use in view handlers);
   - gets the latest MUV model paired with the latest network state;
   - and returns new view built using these states;
-}
type ViewF d model view msg =
    PushF msg d -> Either R.RpdError (model /\ R.Network d) -> view

-- type RenderNode

data Renderer d model view msg
    = Renderer
        { from :: view -- initial view
        , init :: model -- initial state
        , update :: UpdateF d model msg
        , view :: ViewF d model view msg
        }


-- type RendererWithToolkits d model view msg
--     = Renderer d model view msg /\ T.Toolkits d


-- core :: forall d m. Core.Message d -> Message d m
-- core = Core


-- custom :: forall d m. m -> Message d m
-- custom = Custom


extractRpd
    :: forall d model view msg
     . ViewF d model view msg
    -> PushF msg d
    -> R.Rpd (model /\ R.Network d)
    -> Effect view
extractRpd view push rpd =
    R.run onError onSuccess rpd
    where
        onError err = view push $ Left err
        onSuccess res = view push $ Right res


{- render once -}
once
    :: forall d model view msg
     . Renderer d model view msg
    -> R.Rpd (R.Network d)
    -> Effect view
once (Renderer { view, init, update }) rpd =
    extractRpd view neverPush withModel
    where
        neverPush = PushF $ const $ pure unit
        withModel = (/\) init <$> rpd


{- Prepare the rendering cycle with the internal message producer.
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe`, in this case,
   returns the canceler, so it is possible to stop the process.
-}
make
    :: forall d model view msg
     . T.Toolkits d
    -> R.Rpd (R.Network d)
    -> Renderer d model view msg
    -> Effect
        { first :: view
        , next :: Event (Effect view)
        }
make toolkits rpd renderer =
    Event.create >>=
        \event -> pure $ make' event toolkits rpd renderer


{- Prepare the rendering cycle with the custom message producer
   (so, the `Event` with the messages source and
   the function which pushes them to this flow).
   Returns the first view and the event flow with
   all the next views.

   Actually the process starts just when user subscribes
   to the `next` views flow. `Event.subscribe`, in this case,
   returns the canceler, so it is possible to stop the process.

   TODO: do not ask user for `event`, just pushing function.
-}
make'
    :: forall d model view msg
     . { event :: Event (Either msg (C.Command d))
       , push :: Either msg (C.Command d) -> Effect Unit
       }
    -> T.Toolkits d
    -> R.Rpd (R.Network d)
    -> Renderer d model view msg
    -> { first :: view
       , next :: Event (Effect view)
       }
make'
    { event, push }
    toolkits
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
        -- pushCommand
        --     = R.neverPush
        -- update :: msg -> (model /\ R.Network d) -> PushMsg msg -> R.PushCmd d -> R.Rpd (model /\ R.Network d)
        updatePipeline
            :: Either msg (C.Command d)
            -> R.Rpd (model /\ R.Network d)
            -> R.Rpd (model /\ R.Network d)
        updatePipeline msgOrCmd rpd = rpd >>=
            \(model /\ nw) ->
                case msgOrCmd of
                    Left msg -> do
                        -- let _ = DT.spy "msg" msg
                        -- perform user update function, collect user messages
                        let model' /\ actions = update (Left msg) $ model /\ nw
                        -- apply user messages returned from the previous line to the model
                        foldr updatePipeline (pure $ model' /\ nw) actions
                    Right cmd -> do
                        -- let _ = DT.spy "cmd" cmd
                        -- apply the core command to the network
                        nw' <- C.apply cmd (push <<< Right) toolkits nw
                        -- perform the user update function with this core command, collect the returned messages
                        let model' /\ actions = update (Right cmd) $ model /\ nw'
                        -- apply user messages returned from the previous line to the model
                        foldr updatePipeline (pure $ model' /\ nw') actions
        viewer
            :: R.Rpd (model /\ R.Network d)
            -> Effect view
        viewer =
            extractRpd view (PushF push)


{- Run the rendering cycle without any special handling
   (so the rendering results are omitted).

   Returns the canceler. -}
run
    :: forall d view model msg
     . T.Toolkits d
    -> R.Rpd (R.Network d)
    -> Renderer d view model msg
    -> Effect R.Canceler
run toolkits rpd renderer =
    make toolkits rpd renderer >>=
        \{ first, next } -> Event.subscribe next (pure <<< identity)


{- Run the rendering cycle with custom message producer
   (so, the `Event` with the messages source and
   the function which pushes them to this flow).
   Returns the first view and the event flow with
   all the next views.

   TODO: do not ask user for `event`, just pushing function.
-}
run'
    :: forall d view model msg
     . { event :: Event (Either msg (C.Command d))
       , push :: Either msg (C.Command d) -> Effect Unit
       }
    -> T.Toolkits d
    -> R.Rpd (R.Network d)
    -> Renderer d view model msg
    -> Effect R.Canceler
run' event toolkits rpd renderer =
    case make' event toolkits rpd renderer of
        { first, next } -> Event.subscribe next (pure <<< identity)

