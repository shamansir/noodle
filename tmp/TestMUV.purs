module TestMUV where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Either (Either, either)
import Control.Monad.Except.Trans (ExceptT, runExceptT)

import FRP.Event (Event)
import FRP.Event as Event

import Debug.Trace as DT

import UUID as UUID

data Error = Error String

type Program a = ExceptT Error Effect a


data Msg = MsgOne | MsgTwo


instance showMsg :: Show Msg where
    show MsgOne = "MSG-ONE"
    show MsgTwo = "MSG-TWO"


runMUV
    :: forall model view
     . Event Msg
    -> Program model
    -> (Msg -> Program model -> Program model)
    -> (model -> view)
    -> Event (Program view)
runMUV messages init userUpdate userView =
    let
        updates = Event.fold update messages init
        views = view <$> updates
    in views
    where
        update :: Msg -> Program model -> Program model
        update msg model =
            let _ = DT.spy "msg" msg
            in userUpdate msg model
        view :: Program model -> Program view
        view program =
            userView <$> program


main :: Effect Unit
main = do
    { event : messages, push } <- Event.create
    let views = runMUV messages (pure "|") update view
    _ <- Event.subscribe views \pview ->
        runExceptT pview >>= either (const $ log "ERR") log
    push MsgOne
    push MsgTwo
    push MsgTwo
    pure unit
    where
        update msg model =
            model >>= \prev -> do
                uuid <- liftEffect UUID.new
                let _ = DT.spy "uuid" uuid
                pure $ "(" <> show msg <> ":" <> UUID.toString uuid <> ")-" <> prev
        view = identity
        -- view model =
            -- either (const "ERR") identity errOrModel

