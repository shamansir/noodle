module Main where

import Data.Tuple
import Data.Tuple.Nested ((/\))
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C
import Control.Monad.Free (Free)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), Element)
import Data.Foldable (for_, fold)
import Rpd as Rpd
import Signal as S
import Signal.Channel as SC
import Signal.Time as ST
import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
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


main :: ∀ e. Eff (dom :: DOM, console :: C.CONSOLE, channel :: SC.CHANNEL | e) Unit
main = do
  documentType <- document =<< window
  element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  --for_ element (render <@> viewData Bang)
  channel <- SC.channel Bang
  let signal = SC.subscribe channel
  for_ element (\element -> do
    runRpd element signal viewData myNetwork
  )
  SC.send channel $ Str' "test"
  let every300s = (ST.every 300.0) S.~> (\_ -> SC.send channel (Int' 300))
  S.runSignal every300s


-- runRpd :: forall t26 t29 t30.
--       Element
--       -> S.Signal t26
--          -> (t26
--              -> H.Markup ( dom :: DOM
--                     | t30
--                     )
--             )
--             -> R.Network t26
--                -> Eff
--                     ( dom :: DOM
--                     | t29
--                     )
--                     Unit
-- runRpd
--   :: ∀ e d
--    . Element
--   -> S.Signal d
--   -> ?what
--   -> R.Network d
--   -> Eff (dom :: DOM | e) Unit
runRpd targetElm dataSignal renderData network = do
  S.runSignal (map (\t -> render targetElm $ renderData t) dataSignal)
  render targetElm $ Render.network network


viewData :: ∀ e. MyData -> H.Markup e
viewData d =
  H.p $ H.text $ case d of
    Bang -> "Bang"
    Str' str -> "String: " <> str
    Int' int -> "Int: " <> show int


  -- nwSignal <- S.constant myNetwork
  --game <- S.foldp step start signal
  -- H.render $ render myNetwork
  -- S.runSignal (map render $ S.constant myNetwork)
  -- dataSignal <- S.constant Bang
  -- S.runSignal (map renderData dataSignal)
