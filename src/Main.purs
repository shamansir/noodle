module Main where

import Prelude
import Data.Int (floor)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..)) -- , Element)
import Data.Foldable (for_)
import Rpd as R
import Rpd.Render as Render
import Rpd.Render.Html as RenderH
-- import Signal as S
-- import Signal.Channel as SC
-- import Signal.Time as ST
import FRP (FRP)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)


data MyData
  = Bang
  | Str' String String
  | Num' String Int


myNode :: String -> R.LazyNode MyData
myNode nodeId =
  R.node "f"
    [ R.inlet "a" -- WithDefault "a" (Str' (nodeId <> "a") "i")
    , R.inletWithDefault "b" $ Str' (nodeId <> "b") "test"
    , R.inlet' "f" $ map (Num' (nodeId <> "f")) $ interval 5000
    , R.inlet "d" -- (ST.every ST.second S.~> Num' (nodeId <> "d"))
    , R.inlet "e" -- WithDefault "e" (Num' (nodeId <> "e") 3.0)
    ]
    [ R.outlet "c"
    , R.outlet' "x" $ map (Num' (nodeId <> "x")) $ interval 10000
    ]
    -- (\_ -> [ "c" /\ Int' 10 ] )


myNetwork :: R.Network MyData
myNetwork =
  R.network
    [ R.patch "Patch One"
      [ myNode "1"
      , myNode "2"
      ] -- >>> connect (patch.getNode 0) "a" (patch.getNode 1) "b"
    ]


main :: ∀ e. Eff (dom :: DOM, frp :: FRP | e) Unit
main = do
  documentType <- document =<< window
  element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  for_ element (\element ->
    let renderer = RenderH.renderer element
    -- R.run renderer myNetwork
    in renderer myNetwork
    --pure unit
  )


instance showMyData :: Show MyData where
  show Bang = "Bang"
  show (Str' label s) = "Str: " <> label <> "/" <> s
  show (Num' label n) = "Num: " <> label <> "/" <> show n
