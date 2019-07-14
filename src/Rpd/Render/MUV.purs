module Rpd.Render.MUV
    ( Renderer(..)
    , PushF(..)
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

import Rpd.API as R
import Rpd.Network as R
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
     = Either action (Core.Action d c n)
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


data Renderer d c n model view action effect
    = Renderer
        { from :: view -- initial view
        , init :: model -- initial state
        , update :: UpdateF d c n model action effect
        , view :: ViewF d c n model view action
        }


make
    :: forall d c n view
     . Renderer d c n view
    -> T.Toolkit d c n
    -> R.Network d c n
    -> Effect { first :: view, next :: Event view }
make (Renderer { first, viewError, viewValue }) toolkit initialNW = do
    { event : commands, push : pushAction } <- Event.create
    { event : views, push : pushView } <- Event.create
    let
        (updates :: Event (Core.Step d c n)) =
            Event.fold
                (\action step ->
                    case step of
                        Left err -> Left err
                        Right ( model /\ _ ) -> Core.apply toolkit action model)
                commands
                (pure $ initialNW /\ [])
        (models :: Event (Either R.RpdError (R.Network d c n)))
            = ((<$>) fst) <$> updates
    _ <- Event.subscribe updates \step ->
        case step of
            Left err -> pure unit
            Right (model /\ effects) ->
                traverse_ (\eff -> Core.performEffect toolkit pushAction eff model) effects
    _ <- Event.subscribe models (pushView <<< either viewError (viewValue $ PushF pushAction))
    pure { first, next : views }



