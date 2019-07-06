module TestMUV2 where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Data.Array (snoc)
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
    show (Store uuid) = "STORE-UUID: " <> (take 5 $ show uuid) <> "...}"


-- instance showMyEffect :: Show MyEffect where
--     show MsgTwo = "MSG-TWO"
--     show (MakeUUID _) = "MAKE-UUID"


-- type EffectsToPerform myEff msg = Array (myEff msg)
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


update :: Msg -> String -> Program String /\ EffectsToPerform Msg
update msg model =
    let ( prog /\ effects ) = update' msg model
    in ( case prog of
            Left err -> pure (show err <> "-/" <> show msg <> "/")
            Right model' -> pure (model' <> "-(" <> show msg <> ")")
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
    pure (model <> "-<" <> show uuid <> ">")
    /\ []


view :: ViewF String String
view errOrModel =
    either (const "ERR") identity errOrModel


doNoOp :: Unit -> Unit -> Msg
doNoOp _ _ = NoOp


doMsgOne :: Unit -> Msg
doMsgOne _ = MsgOne


doStart :: Unit -> Msg
doStart _ = Start


data MsgList msg = MsgList (Array msg)

-- doStoreUUID :: UUID -> String -> Program String /\ EffectsToPerform Msg
-- doStoreUUID uuid = update $ Store uuid

-- op
--     :: Effect (Program String)
--     -> (String -> Program String /\ EffectsToPerform Msg)
--     -> Effect (Program String)
-- op = ?wh
op :: MsgList Msg -> Msg -> MsgList Msg
op (MsgList arr) msg = MsgList (arr `snoc` msg)
    -- eitherV :: Program String <- effV


initSeq :: MsgList Msg
initSeq = MsgList []


runMUVSequence
    :: (String -> Effect Unit)
    -> MsgList Msg
    -> Effect Unit
runMUVSequence sub (MsgList msgList) = do
    { event : messages, push : pushMsg } <- Event.create
    views <-
        runMUV
            { event : messages, push : pushMsg }
            (pure "")
            { update, view, performEffect }
    cancel <- Event.subscribe views sub
    _ <- traverse_ pushMsg msgList
    --pushMsg Start
    _ <- cancel
    pure unit
    -- let views = Event.fold ?wh messages initialModel
    --pure views
    --where
        -- op :: String -> (String -> Either Error String /\ Array (MyEffect Msg)) -> String
        -- op :: Effect String -> (String -> Either Error String /\ Array (MyEffect Msg)) -> Effect String


runSequence
    :: String
    -> MsgList Msg
    -> (Program String -> Effect Unit)
    -> Effect Unit
runSequence init (MsgList msgList) sub = do
    { event : messages, push : pushMsg } <- Event.create
    let
        updates =
            Event.fold
                (\msg ( prog /\ _ ) ->
                    case prog of
                        Left err -> prog /\ []
                        Right model -> update msg model)
                messages
                (pure init /\ [])
    { event : progs, push : pushProg } <- Event.create
    _ <- Event.subscribe updates \(prog /\ _) ->
        pushProg prog
    _ <- Event.subscribe updates \(prog /\ effects) ->
        performEffects pushMsg performEffect prog effects
    cancel <- Event.subscribe progs sub
    _ <- traverse_ pushMsg msgList
    --pushMsg Start
    _ <- cancel
    pure unit


main :: Effect Unit
main = do
    { event : messages, push : pushMessage } <- Event.create
    views <- runMUV { event : messages, push : pushMessage }
                    (pure "|")
                    { update, view, performEffect }
    stop <- Event.subscribe views log
    -- pushMessage MsgTwo
    pushMessage MsgOne
    pushMessage MsgOne
    pushMessage Start
    _ <- stop
    log "-----"
    log "-----"
    log "-----"
    _ <- runMUVSequence log
        $ initSeq
            `op` doNoOp unit unit
            `op` doMsgOne unit
            `op` doStart unit
            `op` doNoOp unit unit
    log "DONE"
    pure unit


