module Rpd.Render.Minimal
    ( Renderer(..)
    , PushCmd(..)
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
import Rpd.API.Command (Command) as C
import Rpd.API.CommandApply (apply, Step) as C
import Rpd.API.CommandApply (performEffect) as R
import Rpd.Network (Network) as R
import Rpd.Util (Canceler) as R
import Rpd.Toolkit (Toolkit) as T


-- data RendererCommand d rcmd
--     = Core (C.Command d)
--     | Renderer rcmd


-- data PushCmd d rcmd =
--     PushCmd (RendererCommand d rcmd -> Effect Unit)


-- type RenderF d rcmd view
--     =  PushCmd (RendererCommand d rcmd -> Effect Unit)
--     -> Either R.RpdError (R.Network d)
--     -> view

data PushCmd d c n =
    PushCmd (C.Command d c n -> Effect Unit)


data Renderer d c n view
    = Renderer
        { first :: view
        , viewError :: R.RpdError -> view
        , viewValue :: PushCmd d c n -> R.Network d c n -> view
        }


neverPush :: forall d c n. PushCmd d c n
neverPush = PushCmd $ const $ pure unit


make
    :: forall d c n view
     . R.Network d c n
    -> Renderer d c n view
    -> Effect { first :: view, next :: Event view }
make initialNW (Renderer { first, viewError, viewValue }) = do
    { event : commands, push : pushCmd } <- Event.create
    { event : views, push : pushView } <- Event.create
    let
        (updates :: Event (C.Step d c n)) =
            Event.fold
                (\cmd step ->
                    case step of
                        Left err -> Left err
                        Right ( model /\ _ ) -> C.apply cmd model)
                commands
                (pure $ initialNW /\ [])
        (models :: Event (Either R.RpdError (R.Network d c n)))
            = ((<$>) fst) <$> updates
    _ <- Event.subscribe updates \step ->
        case step of
            Left err -> pure unit
            Right (model /\ effects) ->
                traverse_ (\eff -> R.performEffect eff pushCmd model) effects
    _ <- Event.subscribe models (pushView <<< either viewError (viewValue $ PushCmd pushCmd))
    pure { first, next : views }



