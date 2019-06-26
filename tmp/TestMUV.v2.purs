module TestMUV2 where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Data.String (take)
import Data.Either (Either(..), either)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)

import Control.Monad.Except.Trans (ExceptT, runExceptT)

import FRP.Event (Event)
import FRP.Event as Event

import Debug.Trace as DT

import UUID (UUID)
import UUID as UUID

newtype Error = Error String

type Program a = Either Error a


data Msg
    = NoOp
    | MsgOne
    | Start
    | Store UUID


data MyEffect msg
    = MsgTwo
    | MakeUUID (UUID -> msg)


derive newtype instance showError :: Show Error

instance showMsg :: Show Msg where
    show NoOp = "NO-OP"
    show MsgOne = "MSG-ONE"
    show Start = "START"
    show (Store uuid) = "STORE-UUID: " <> (take 5 $ show uuid)


-- instance showMyEffect :: Show MyEffect where
--     show MsgTwo = "MSG-TWO"
--     show (MakeUUID _) = "MAKE-UUID"


type EffectsToPerform msg = Array (MyEffect msg)
type PerformEffectF model msg = MyEffect msg -> (msg -> Effect Unit) -> Program model -> Effect Unit
type UpdateF model msg = msg -> model -> Program model /\ EffectsToPerform msg
type ViewF model view = Program model -> view
type PushF msg = msg -> Effect Unit


runMUV
    :: forall model view msg
     . { event :: Event msg, push :: msg -> Effect Unit }
    -> Program model
    -> { update :: UpdateF model msg
       , view :: ViewF model view
       , performEffect :: PerformEffectF model msg
       }
    -> Effect (Event view)
runMUV
    { event : messages, push : pushMessage }
    init
    { update : userUpdate, view : userView, performEffect : userPerformEff } = do
    let
        updates =
            Event.fold
                (\msg ( prog /\ _ ) ->
                    case prog of
                        Left err -> prog /\ []
                        Right model -> userUpdate msg model)
                messages
                (init /\ [])
    { event : views, push : pushView } <- Event.create
    _ <- Event.subscribe updates \(prog /\ _) ->
        pushView $ userView prog
    _ <- Event.subscribe updates \(prog /\ effects) ->
        performEffects pushMessage userPerformEff prog effects
    pure views


performEffects
    :: forall model msg. PushF msg -> PerformEffectF model msg -> Program model -> EffectsToPerform msg -> Effect Unit
performEffects pushMsg performEff model effects =
    traverse_ (\eff -> performEff eff pushMsg model) effects


performEffect :: forall model. MyEffect Msg -> PushF Msg -> Program model -> Effect Unit
performEffect (MakeUUID f) pushMsg program = do
    uuid <- UUID.new
    pushMsg $ f uuid
performEffect MsgTwo _ program = pure unit


main :: Effect Unit
main = do
    { event : messages, push : pushMessage } <- Event.create
    views <- runMUV { event : messages, push : pushMessage }
                    (pure "|")
                    { update, view, performEffect }
    _ <- Event.subscribe views log
    -- pushMessage MsgTwo
    pushMessage MsgOne
    pushMessage MsgOne
    pushMessage Start
    pure unit
    where
        update :: Msg -> String -> Program String /\ EffectsToPerform Msg
        update msg model =
            let ( prog /\ effects ) = update' msg model
            in ( case prog of
                    Left err -> pure ("(" <> show msg <> ")-" <> show err)
                    Right model' -> pure ("(" <> show msg <> ")-" <> model')
               ) /\ effects
        update' :: Msg -> String -> Program String /\ EffectsToPerform Msg
        -- update' (MakeUUID f) model = pure model /\ [ f <$> UUID.new ]
        update' NoOp model = pure model /\ [ ]
        update' MsgOne model = pure model /\ [ MsgTwo ]
        update' Start model = pure model /\
            [ MakeUUID Store
            , MakeUUID Store
            , MakeUUID Store
            ]
        update' (Store uuid) model =
            pure ("<" <> show uuid <> ">-" <> model)
            /\ []
        -- update msg m = m
        --     case model of
        --     model >>= \prev -> do
        --         uuid <- liftEffect UUID.new
        --         -- let _ = DT.spy "uuid" uuid
        --         pure $ "(" <> show msg <> ":" <> UUID.toString uuid <> ")-" <> prev
        view errOrModel =
            either (const "ERR") identity errOrModel

