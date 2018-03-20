module Main where

import Data.Tuple
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C
import Control.Monad.Free (Free)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), Element)
import Data.Foldable (for_, fold)
import Data.Tuple.Nested ((/\))
import Render as Render
import Rpd as R
import Signal as S
import Signal.Channel (CHANNEL)
import Signal.Channel as SC
import Signal.Time as ST
import Text.Smolder.HTML as H
import Text.Smolder.Markup ((#!), on)
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM (render)

data MyData
  = Bang
  | Str' String
  | Int' Int


myNode :: R.LazyNode MyData
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
      ] -- >>> connect (patch.getNode 0) "a" (patch.getNode 1) "b"
    ]


main :: ∀ e. Eff (dom :: DOM, console :: C.CONSOLE, channel :: SC.CHANNEL | e) Unit
main = do
  documentType <- document =<< window
  element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  channel <- SC.channel Bang
  let signal = SC.subscribe channel
  --for_ element (render <@> viewData Bang)
  for_ element (\element -> do
    -- runRpd element signal viewData myNetwork
    runRpd element myNetwork
  )
  SC.send channel $ Str' "test"
  -- let every300s = (ST.every 300.0) S.~> (\_ -> SC.send channel (Int' 300))
  -- S.runSignal every300s


runRpd :: forall e d. Element -> R.Network d -> Eff ( dom :: DOM, channel :: SC.CHANNEL | e ) Unit
runRpd targetElm network = do
  channel <- SC.channel myNetwork -- just pass updated network through network
  let signal = SC.subscribe channel
  let sender = (\network -> do SC.send channel network)
  -- S.folp
  S.runSignal (signal S.~> (\network -> render targetElm $ Render.network network sender))
-- runRpd targetElm dataSignal renderData network = do
  -- S.runSignal (map (\t -> render targetElm $ renderData t) dataSignal)
  -- render targetElm $ Render.network network sender

      -- render targetElm $ Render.update evt network)


-- viewData :: forall e. MyData -> H.Markup (Render.Listener e)
-- viewData d =
--   H.p $ H.text $ case d of
--     Bang -> "Bang"
--     Str' str -> "String: " <> str
--     Int' int -> "Int: " <> show int
