module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..)) -- , Element)
import Data.Foldable (for_)
--import Render (renderer)
import Render as Render
import Rpd as R
import Signal as S
import Signal.Channel as SC
import Signal.Time as ST


data MyData
  = Bang
  | Str' String
  | Num' Number


myNode :: R.LazyNode MyData
myNode =
  R.node "f"
    [ R.inlet' "a" (Str' "i")
    , R.inlet' "b" (Str' "test") -- FIXME: "test" is not shown!
    , R.inlet "f" (ST.every (2.0 * ST.second) S.~> Num')  -- FIXME: also not shown!
    , R.inlet "d" (ST.every ST.second S.~> Num')
    , R.inlet' "e" (Num' 3.0) -- FIXME: "3.0" is not shown!
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


main :: ∀ e. Eff (dom :: DOM, channel :: SC.CHANNEL | e) Unit
main = do
  documentType <- document =<< window
  element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  for_ element (\element -> do
    let renderer = Render.renderer element
    liftEff $ R.run renderer myNetwork
  )


instance showMyData :: Show MyData where
  show Bang = "Bang"
  show (Str' s) = "Str: " <> s
  show (Num' n) = "Num: " <> show n
