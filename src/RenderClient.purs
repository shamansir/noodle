module Web.RenderClient where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console
import Foreign as F
import Control.Monad.Except.Trans as T

import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.String as String
import Data.Array as Array

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Halogen.Query.Event (eventListener)
import Web.Event.EventTarget as ET

import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes as WS
import Web.Socket.Event.MessageEvent as WSMsg



main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type State = Unit


initialState :: forall i. i -> State
initialState = const unit


data Action
  = Initialize
  | OnWsMessage H.SubscriptionId WSMsg.MessageEvent


component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }


render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
   HH.div_
      [ HH.span [] [ HH.text "foo" ]
      , HH.canvas
        [ HP.id "hydra-canvas"
        ]
      ]


handleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
      ws <- liftEffect $ WS.create "ws://localhost:9999" []
      liftEffect $ Console.log "connected"
      let wset = WS.toEventTarget ws
      H.subscribe' \sid ->
        eventListener
          WS.onMessage
          wset
          (map (OnWsMessage sid) <<< WSMsg.fromEvent)
      {-
      closeListener <- liftEffect $ ET.eventListener $ const $ Console.log "close"
      messageListener <- liftEffect $ ET.eventListener $ \evt ->
          case WSMsg.fromEvent evt of
            Just msgevt -> do
              let messageData = WSMsg.data_ msgevt
              str <- T.runExceptT $ F.readString messageData
              Console.log $ either (Array.fromFoldable >>> map F.renderForeignError >>> String.joinWith ", ") identity str
              pure unit
            Nothing ->
              pure unit
      liftEffect $ ET.addEventListener WS.onClose closeListener true wset
      liftEffect $ ET.addEventListener WS.onMessage messageListener true wset
      -}
      pure unit
  OnWsMessage _ msgevt -> do
      let messageData = WSMsg.data_ msgevt
      str <- T.runExceptT $ F.readString messageData
      liftEffect $ Console.log $ either (Array.fromFoldable >>> map F.renderForeignError >>> String.joinWith ", ") identity str
      pure unit



{- const url = "ws://localhost:9999";
const connection = new WebSocket(url);
let introduceStop;
connection.onopen = () => {
  // After connection opens send message to server.
  connection.send("CLIENT: PONG");
// After 10 seconds close web socket connection.
  introduceStop = setTimeout(function () {
    console.log(
      `${new Date().toLocaleTimeString()}: Client closing WebSocket connection`
    );
    connection.close();
  }, 10000);
};
// Catch error while web socket connection is open.
// After `onerror`, `onclose` will be triggered.
connection.onerror = (error) => {
  console.log(`Web socket error: ${error}`);
};
// Keep reading messages from server.
connection.onmessage = (event) => {
  console.log(`${new Date().toLocaleTimeString()} ${event.data}`);
};
// Clear things once web socket connection is closed.
connection.onclose = () => {
  clearTimeout(introduceStop);
  console.log("Web Socket connection properly closed.");
}; -}