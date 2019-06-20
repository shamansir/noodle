module TestMUV where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Data.Either (Either(..), either)
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
    _ <- Event.subscribe updates \progAndEffects ->
        performEffects pushMessage progAndEffects >>= pushView <<< userView
    pure views
    where
        performEffects
            :: (Msg -> Effect Unit)
            -> Program model /\ EffectsToPerform
            -> Effect (Program model)
        performEffects pushMsg (prog /\ effects) = do
            traverse_ ((=<<) pushMsg) effects
            pure prog


main :: Effect Unit
main = do
    { event : messages, push : pushMessage } <- Event.create
    views <- runMUV { event : messages, push : pushMessage } (pure "|") update view
    _ <- Event.subscribe views log
    pushMessage MsgOne
    pushMessage MsgTwo
    pushMessage MsgTwo
    pushMessage Start
    pure unit
    where
        update msg (Right model) =
            case update' msg (Right model) of
                (Right model' /\ effects) ->
                    Right ("(" <> show msg <> ")-" <> model')
                    /\ effects
                v -> v
        update msg prog = update' msg prog
        update' (MakeUUID f) prog = prog /\ [ f <$> UUID.new ]
        update' MsgTwo prog = prog /\ [ pure MsgOne ]
        update' Start prog = prog /\
            [ pure $ MakeUUID Store
            , pure $ MakeUUID Store
            , pure $ MakeUUID Store
            ]
        update' (Store uuid) (Right model) =
            Right ("<" <> show uuid <> ">-" <> model)
            /\ []
        update' _ prog = prog /\ []
        -- update msg m = m
        --     case model of
        --     model >>= \prev -> do
        --         uuid <- liftEffect UUID.new
        --         -- let _ = DT.spy "uuid" uuid
        --         pure $ "(" <> show msg <> ":" <> UUID.toString uuid <> ")-" <> prev
        view errOrModel =
            either (const "ERR") identity errOrModel

