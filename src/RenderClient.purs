module Web.RenderClient where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console
import Foreign as F
import Control.Monad.Except.Trans as T
import Control.Monad.State as State

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

import Toolkit.Hydra2.Engine as Hydra


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


data State
  = Connecting
  | Ready
  | Error String


initialState :: forall i. i -> State
initialState = const Connecting


data Action
  = Initialize
  | OnWsOpen H.SubscriptionId
  | OnWsMessage H.SubscriptionId WSMsg.MessageEvent
  | OnWsClose H.SubscriptionId
  | OnWsError H.SubscriptionId
  | ParsingError String
  | Prepare
  | Render String


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
render =
  case _ of
    Connecting -> HH.span [] [ HH.text "Connecting" ]
    Ready -> HH.div_
          [ HH.span [] [ HH.text "Ready" ]
          , HH.canvas
            [ HP.id "hydra-canvas"
            ]
          ]
    Error error -> HH.span [] [ HH.text $ "Error: " <> error ]

handleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
      ws <- liftEffect $ WS.create "ws://localhost:9999" []
      liftEffect $ Console.log "connected"
      let wset = WS.toEventTarget ws
      H.subscribe' \sid ->
        eventListener WS.onMessage wset (map (OnWsMessage sid) <<< WSMsg.fromEvent)
      H.subscribe' \sid ->
        eventListener WS.onOpen wset (Just <<< const (OnWsOpen sid))
      H.subscribe' \sid ->
        eventListener WS.onClose wset (Just <<< const (OnWsClose sid))
      H.subscribe' \sid ->
        eventListener WS.onError wset (Just <<< const (OnWsError sid))
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
      liftEffect $ Console.log $ either errorsToString identity str
      _ <- str # either (errorsToString >>> ParsingError >>> handleAction) (messageToAction >>> handleAction)
      pure unit
  OnWsOpen _ -> do
      liftEffect $ Console.log "open"
      pure unit
  OnWsClose _ -> do
      liftEffect $ Console.log "close"
      pure unit
  OnWsError _ -> do
      State.put $ Error "WebSocket error"
      liftEffect $ Console.log "error"
      pure unit
  ParsingError error -> do
      State.put $ Error error
      liftEffect $ Console.log $ "error: " <> error
      pure unit
  Prepare -> do
      liftEffect $ Console.log "prepare"
      State.put Ready
      liftEffect $ Hydra.init $ Hydra.TargetCanvas "hydra-canvas"
      pure unit
  Render what -> do
      liftEffect $ Console.log $ "render" <> what
      liftEffect $ Hydra.evaluate $ Hydra.HydraCode what
      pure unit
  where
      errorsToString =
        Array.fromFoldable >>> map F.renderForeignError >>> String.joinWith ", "
      messageToAction str = case str of
        "ACK" -> Prepare
        _ -> Render str






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