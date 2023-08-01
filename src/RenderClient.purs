module Web.RenderClient where

import Prelude

import Effect (Effect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


data Action = Increment | Decrement


component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1


{-
render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.p_
        [ HH.text $ "You clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
-}



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