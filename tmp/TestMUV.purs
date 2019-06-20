module TestMUV where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Data.Either (Either, either)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)

import Control.Monad.Except.Trans (ExceptT, runExceptT)

import FRP.Event (Event)
import FRP.Event as Event

import Debug.Trace as DT

import UUID (UUID)
import UUID as UUID

data Error = Error String

type Program a = Either Error a


data Msg
    = MsgOne
    | MsgTwo
    | Start
    | MakeUUID (UUID -> Msg)
    | Store UUID



instance showMsg :: Show Msg where
    show MsgOne = "MSG-ONE"
    show MsgTwo = "MSG-TWO"
    show Start = "START"
    show (Store uuid) = "STORE-UUID: " <> show uuid
    show (MakeUUID _) = "MAKE-UUID"


type EffectsToPerform = Array (Effect Msg)


runMUV
    :: forall model view
     . { event :: Event Msg, push :: Msg -> Effect Unit }
    -> Program model
    -> (Msg -> Program model -> Program model /\ EffectsToPerform)
    -> (Program model -> view)
    -> Effect (Event view)
runMUV { event : messages, push : pushMessage } init userUpdate userView = do
    let
        updates =
            Event.fold
                (\msg (model /\ _) -> userUpdate msg model)
                messages
                (init /\ [])
    { event : views, push : pushView } <- Event.create
    _ <- Event.subscribe updates (performEffects pushView pushMessage)
    --     views = view <$> updates
    -- in views
    pure views
    where
        update
            :: Msg
            -> Program model /\ EffectsToPerform
            -> Program model /\ EffectsToPerform
        update msg (model /\ _) =
            -- let _ = DT.spy "msg" msg
            userUpdate msg model
        view :: Program model -> view
        view program =
            userView program
        performEffects
            :: (view -> Effect Unit)
            -> (Msg -> Effect Unit)
            -> Program model /\ EffectsToPerform
            -> Effect Unit
        performEffects pushView pushMsg (model /\ effects) =
            traverse_ identity effects


main :: Effect Unit
main = do
    { event : messages, push : pushMessage } <- Event.create
    views <- runMUV { event : messages, push : pushMessage } (pure "|") update view
    _ <- Event.subscribe views log
    pushMessage MsgOne
    pushMessage MsgTwo
    pushMessage MsgTwo
    pure unit
    where
        update msg model = model /\ []
        -- update msg m = m
        --     case model of
        --     model >>= \prev -> do
        --         uuid <- liftEffect UUID.new
        --         -- let _ = DT.spy "uuid" uuid
        --         pure $ "(" <> show msg <> ":" <> UUID.toString uuid <> ")-" <> prev
        view errOrModel =
            either (const "ERR") identity errOrModel

