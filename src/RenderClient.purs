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
import Data.Tuple.Nested ((/\), type (/\))
import Data.MediaType (MediaType(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Halogen.Query.Event (eventListener)

import Web.Event.Event (EventType(..))
import Web.Event.EventTarget as ET
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Emitters  as Emitters

import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes as WS
import Web.Socket.Event.MessageEvent as WSMsg

import Web.File.Url as Web
import Web.File.Blob as Blob

import Toolkit.Hydra2.Engine as Hydra
import Toolkit.Hydra2.Engine (HydraCode(..))


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


data Connection
  = Connecting
  | Ready
  | Error String


newtype BlobUrl = BlobUrl String


type State =
  { window :: { w :: Int, h :: Int }
  , connection :: Connection
  , error :: Maybe String
  , currentSceneBlob :: Maybe BlobUrl
  }


initialState :: forall i. i -> State
initialState = const
  { window : { w : 0, h : 0 }
  , connection : Connecting
  , error : Nothing
  , currentSceneBlob : Nothing
  }


data Action
  = Initialize
  | Connect
  | OnWsOpen H.SubscriptionId
  | OnWsMessage H.SubscriptionId WSMsg.MessageEvent
  | OnWsClose H.SubscriptionId
  | OnWsError H.SubscriptionId
  | ParsingError String
  | Prepare
  | Render String
  | WindowResize H.SubscriptionId { w :: Int, h :: Int }
  | Save


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
  case state.connection of
    Connecting -> HH.span [ HP.id "status" ] [ HH.text "Connecting" ]
    Ready ->
      HH.div_
          [ HH.canvas
            [ HP.id "hydra-canvas"
            , HP.width state.window.w
            , HP.height state.window.h
            ]
          , HH.span
            [ HP.id "status", HP.class_ $ H.ClassName "ready" ]
            [ HH.text "Ready" ]
          , case state.currentSceneBlob of
                Just (BlobUrl blobUrl) ->
                  HH.a
                    [ HP.id "save"
                    , HP.download "shader_scene.html"
                    , HP.href blobUrl
                    ]
                    [ HH.text "Save"
                    ]
                Nothing -> HH.div_ []
          ]
    Error error ->
      HH.div
        []
        [ HH.span [ HP.id "status" ] [ HH.text $ "Error: " <> error ]
        , HH.button
          [ HP.id "retry", HE.onClick $ const Connect ]
          [ HH.text "Retry" ]
        ]


handleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize ->
      handleAction Connect
  Connect -> do
      ws <- liftEffect $ WS.create "ws://localhost:9999" []
      liftEffect $ Console.log "connected"
      let
        wset = WS.toEventTarget ws
      H.subscribe' \sid ->
        eventListener WS.onMessage wset (map (OnWsMessage sid) <<< WSMsg.fromEvent)
      H.subscribe' \sid ->
        eventListener WS.onOpen wset (Just <<< const (OnWsOpen sid))
      H.subscribe' \sid ->
        eventListener WS.onClose wset (Just <<< const (OnWsClose sid))
      H.subscribe' \sid ->
        eventListener WS.onError wset (Just <<< const (OnWsError sid))
  Prepare -> do
      liftEffect $ Console.log "prepare"
      innerWidth <- H.liftEffect $ Window.innerWidth =<< window
      innerHeight <- H.liftEffect $ Window.innerHeight =<< window
      -- H.modify_ _ { windowSize = innerWidth /\ innerHeight }
      windowResize <- H.liftEffect Emitters.windowDimensions
      H.subscribe' $ \sid -> WindowResize sid <$> windowResize
      State.modify_ $ _ { window = { w : innerWidth, h : innerHeight } }
      liftEffect $ Hydra.init $ Hydra.TargetCanvas "hydra-canvas"
      State.modify_ $ _ { connection = Ready }
  WindowResize _ newSize -> do
      liftEffect $ Console.log $ show newSize
      State.modify_ $ _ { window = newSize }
  Render what -> do
      liftEffect $ Console.log $ "render" <> what
      liftEffect $ Hydra.evaluate $ HydraCode what
      let blob = Blob.fromString (buildHtmlWith $ HydraCode what) $ MediaType "text/html"
      objectUrl <- liftEffect $ Web.createObjectURL blob
      State.modify_ $ _ { currentSceneBlob = Just $ BlobUrl objectUrl }
  OnWsMessage _ msgevt -> do
      let messageData = WSMsg.data_ msgevt
      str <- T.runExceptT $ F.readString messageData
      liftEffect $ Console.log $ either errorsToString identity str
      _ <- str # either (errorsToString >>> ParsingError >>> handleAction) (messageToAction >>> handleAction)
      pure unit
  OnWsOpen _ -> do
      liftEffect $ Console.log "open"
  OnWsClose _ -> do
      liftEffect $ Console.log "close"
  OnWsError _ -> do
      State.modify_ $ _ { error = Just "WebSocket error" }
      liftEffect $ Console.log "error"
  ParsingError error -> do
      State.modify_ $ _ { error = Just "Parsing error" }
      liftEffect $ Console.log $ "error: " <> error
  Save ->
      pure unit
      -- TODO
  where
      errorsToString =
        Array.fromFoldable >>> map F.renderForeignError >>> String.joinWith ", "
      messageToAction str = case str of
        "ACK" -> Prepare
        _ -> Render str



buildHtmlWith :: HydraCode -> String
buildHtmlWith (HydraCode hydraCode) =
  """
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <meta
      name="viewport"
      content="width=device-width, initial-scale=1,shrink-to-fit=no"
    />
    <!-- import the latest version of hydra synth-->
    <script src="https://unpkg.com/hydra-synth"></script>

    <title>Produced with Noodle</title>

    <style>
        html, body {
            margin: 0;
            padding: 0;
            height: 100vh;
        }

        body {
            width: 100%;
        }

        canvas {
            width: 100%;
            height: 100%;
        }
    </style>
  </head>
  <body>
    <canvas id="hydra-canvas"></canvas>
    <script>
      var hydra = new Hydra({
        canvas: document.getElementById("hydra-canvas"),
        detectAudio: false
      });

      hydra.setResolution(window.innerWidth, window.innerHeight);
    </script>
    <script>
""" <> hydraCode <> """
    </script>
  </body>
</html>
  """



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