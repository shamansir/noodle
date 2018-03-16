module Main where

import Data.Tuple
import Data.Tuple.Nested ((/\))
import Prelude

import Data.Foldable (for_, fold)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C
import Control.Monad.Free (Free)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), Element)
import DOM.Event.Event (Event)
import DOM.Event.EventTarget (EventListener, eventListener)

import Signal as S
import Signal.Channel as SC
import Signal.Time as ST

import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
import Text.Smolder.Markup ((#!), on)
import Text.Smolder.Renderer.DOM (render)

import Rpd as R
import Render as Render

data MyData
  = Bang
  | Str' String
  | Int' Int


myNode :: R.Node MyData
myNode =
  R.node "f"
    [ R.inlet' "a" (Str' "i")
    , R.inlet' "b" (Int' 2)
    ]
    [ R.outlet "c"
    ]
    -- (\_ -> [ "c" /\ Int' 10 ] )


myNetwork :: R.Network MyData
myNetwork =
  R.network
    [ R.patch "Patch One"
      [ myNode
      , myNode
      ]
    ]


a :: Int -> Int
a x =
  let a' = 3 :: Int
  in a' + x


data RpdEvent
  = Connect
  | Move
  | Resize


data MyEvent = EventOne | EventTwo

instance showMyEvent :: Show MyEvent where
  show EventOne = "Event One"
  show EventTwo = "Event EventTwo"

type Listener e = MyEvent -> Eff ( console :: C.CONSOLE | e ) Unit

type Model = Int

update :: forall e. MyEvent -> Model -> H.Markup e
update evt model =
     H.div $ H.text "Test"

render' :: forall e. Model -> H.Markup MyEvent
render' model =
    H.div
      #! on "click" EventOne
      $ H.text "Foo"

listener :: forall e. Event -> Eff ( console :: C.CONSOLE | e ) Unit
listener evt =
  C.log "got the event"

listenerHtml :: forall e. Element -> Event -> Eff ( dom :: DOM | e ) Unit
listenerHtml trg _ =
  render trg (H.div $ H.text "Got the event")

listenerHtml' :: forall e. Element -> MyEvent -> Event -> Eff ( dom :: DOM | e ) Unit
listenerHtml' trg _ _ =
  render trg (H.div $ H.text "Got the event")

listenerHtml'' :: forall e. Element -> MyEvent -> Eff ( dom :: DOM | e ) Unit
listenerHtml'' trg _ =
  render trg (H.div $ H.text "Got the event")


main :: ∀ e. Eff (dom :: DOM, console :: C.CONSOLE | e) Unit
main = do
  documentType <- document =<< window
  element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  for_ element (\element -> do
    render element (H.div #! on "click" (eventListener listener) $ H.text "AAAA")
    render element (H.div #! on "click" (eventListener $ listenerHtml element) $ H.text "BBB")
    render element (H.div #! on "click" (eventListener $ listenerHtml' element EventOne) $ H.text "CCC")
    render element $ testF element
    render element $ testF' element
  )

testF :: forall e. Element -> H.Markup (EventListener ( dom :: DOM | e ))
testF element
  = H.div #! on "click" (eventListener $ listenerHtml' element EventOne) $ H.text "DDD"

testF' :: forall e. Element -> H.Markup (EventListener ( dom :: DOM | e ))
testF' element
  = H.div #! on "click" (eventListener $ const $ listenerHtml'' element EventOne) $ H.text "EEE"

-- main :: ∀ e. Eff (dom :: DOM, console :: C.CONSOLE, channel :: SC.CHANNEL | e) Unit
-- main = do
--   documentType <- document =<< window
--   element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
--   channel <- SC.channel Bang
--   let signal = SC.subscribe channel
--   --for_ element (render <@> viewData Bang)
--   for_ element (\element -> do
--     runRpd element signal viewData myNetwork
--   )
--   SC.send channel $ Str' "test"
--   -- let every300s = (ST.every 300.0) S.~> (\_ -> SC.send channel (Int' 300))
--   -- S.runSignal every300s


-- runRpd targetElm dataSignal renderData network = do
--   S.runSignal (map (\t -> render targetElm $ renderData t) dataSignal)
--   render targetElm $ Render.network network


-- viewData :: forall e. MyData -> H.Markup (Render.Listener e)
-- viewData d =
--   H.p $ H.text $ case d of
--     Bang -> "Bang"
--     Str' str -> "String: " <> str
--     Int' int -> "Int: " <> show int
