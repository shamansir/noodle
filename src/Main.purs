module Main where

import Data.Tuple
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..))
import Data.Foldable (for_, fold)
import Rpd as Rpd
import Signal as S
import Signal.Channel as SC
import Signal.Time as ST
import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
import Text.Smolder.Renderer.DOM (render)


data MyData
  = Bang
  | Str' String
  | Int' Int


type ProcessF d = (Array (Tuple String d) -> Array (Tuple String d))

type AdaptF d = (d -> d)

data Network d = Network (Array (Patch d))
data Patch d = Patch String (Array (Node d)) (Array Link)
--data Patch = Patch String (Array (Node Unit Unit)) (Array Link)
data Node d = Node String (Array (Inlet d)) (Array (Outlet d)) (ProcessF d) -- (S.Signal Unit) add node type just for tagging?
--data Node a b = Node String (a -> b)
--data Node a b = Node String (Map String Unit -> Map String Unit)
data Inlet d = Inlet String (S.Signal d) -- change to channel
--data Inlet d = Inlet String (Maybe (AdaptF d))
data Outlet d = Outlet String (S.Signal d)
data Link = Link


myNode =
  Node "f"
    [ Inlet "a" (S.constant (Str' "i"))
    , Inlet "b" (S.constant (Int' 2))
    ]
    [ Outlet "c" (S.constant (Int' 6))
    ]
    (\_ -> [ (Tuple "c" (Int' 10) ) ] )


--myNetwork :: Network MyData
myNetwork =
  Network
    [ Patch "p"
      [ myNode
      , myNode
      ]
      []
    ]


a :: Int -> Int
a x =
  let a' = 3 :: Int
  in a' + x


listen :: Network MyData -> S.Signal MyData
listen nw = S.constant Bang


viewNetwork :: forall e. Network MyData -> H.Markup e
viewNetwork nw =
  H.p $ H.text (fold ["A", "B"] <> show (a 5))

viewData :: forall e. MyData -> H.Markup e
viewData d =
  H.p $ H.text $ case d of
    Bang -> "Bang"
    Str' str -> "String: " <> str
    Int' int -> "Int: " <> show int


view :: ∀ e. String -> H.Markup e
view s =
  H.div
    $ H.text s


-- main function with a custom patch

-- data MyNodeType = NumNode | StrNode

-- data MyInletType = NumInlet | StrInlet


-- main :: Eff (console :: C.CONSOLE, channel :: SC.CHANNEL) Unit
-- main = void do
--     C.log "test"
    --Rpd.run [] Rpd.network
    -- Rpd.run
    --     ( Rpd.createNetwork "a" : Rpd.createNetwork "b" : [] )
    --     (\s -> map show s S.~> C.log)


main :: ∀ e. Eff (dom :: DOM, console :: C.CONSOLE, channel :: SC.CHANNEL | e) Unit
main = do
  documentType <- document =<< window
  element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  --for_ element (render <@> viewData Bang)
  channel <- SC.channel Bang
  let signal = SC.subscribe channel
  for_ element (\element ->
    S.runSignal (map (\t -> render element $ viewData t) signal)
  )
  SC.send channel $ Str' "test"
  let every300s = (ST.every 300.0) S.~> (\_ -> SC.send channel (Int' 300))
  S.runSignal every300s

  -- for_ element (S.runSignal (map (render ) S.constant myNetwork))


  -- nwSignal <- S.constant myNetwork
  --game <- S.foldp step start signal
  -- H.render $ render myNetwork
  -- S.runSignal (map render $ S.constant myNetwork)
  -- dataSignal <- S.constant Bang
  -- S.runSignal (map renderData dataSignal)
