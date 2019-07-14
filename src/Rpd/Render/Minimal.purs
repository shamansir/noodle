module Rpd.Render.Minimal
    ( Renderer(..)
    , PushAction(..)
    , make
    ) where


import Prelude

import Data.Either (Either(..), either)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Traversable (traverse_)

import Effect (Effect)

import FRP.Event (Event)
import FRP.Event as Event

import Rpd.API (RpdError) as R
import Rpd.API.Action (Action) as C
import Rpd.API.Action.Apply (Step, apply, performEffect) as C
import Rpd.Network (Network) as R
import Rpd.Toolkit (Toolkit) as T


-- data RendererAction d rcmd
--     = Core (C.Action d)
--     | Renderer rcmd


-- data PushAction d rcmd =
--     PushAction (RendererAction d rcmd -> Effect Unit)


-- type RenderF d rcmd view
--     =  PushAction (RendererAction d rcmd -> Effect Unit)
--     -> Either R.RpdError (R.Network d)
--     -> view

data PushAction d c n =
    PushAction (C.Action d c n -> Effect Unit)


data Renderer d c n view
    = Renderer
        { first :: view
        , viewError :: R.RpdError -> view
        , viewValue :: PushAction d c n -> R.Network d c n -> view
        }


neverPush :: forall d c n. PushAction d c n
neverPush = PushAction $ const $ pure unit


make
    :: forall d c n view
     . Renderer d c n view
    -> T.Toolkit d c n
    -> R.Network d c n
    -> Effect { first :: view, next :: Event view }
make (Renderer { first, viewError, viewValue }) toolkit initialNW = do
    { event : actions, push : pushAction } <- Event.create
    { event : views, push : pushView } <- Event.create
    let
        (updates :: Event (C.Step d c n)) =
            Event.fold
                (\cmd step ->
                    case step of
                        Left err -> Left err
                        Right ( model /\ _ ) -> C.apply toolkit cmd model)
                actions
                (pure $ initialNW /\ [])
        (models :: Event (Either R.RpdError (R.Network d c n)))
            = ((<$>) fst) <$> updates
    _ <- Event.subscribe updates \step ->
        case step of
            Left err -> pure unit
            Right (model /\ effects) ->
                traverse_ (\eff -> C.performEffect toolkit pushAction eff model) effects
    _ <- Event.subscribe models (pushView <<< either viewError (viewValue $ PushAction pushAction))
    pure { first, next : views }



