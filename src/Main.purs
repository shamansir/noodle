module Main where

import Prelude
import Data.Int (floor)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
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
import FRP.Event (Event, create, subscribe, fold)
import FRP.Event.Time (interval)
import Rpd.Flow (flow)


data MyData
  = Bang
  | Str' String String
  | Num' String Int


myNode :: String -> R.LazyNode MyData
myNode nodeId =
  R.node "f"
    [ R.inlet "a" -- WithDefault "a" (Str' (nodeId <> "a") "i")
    , R.inletWithDefault "b" $ Str' (nodeId <> "b") "test"
    , R.inlet' "f" $ flow $ map (Num' (nodeId <> "f")) $ interval 5000
    , R.inlet "d" -- (ST.every ST.second S.~> Num' (nodeId <> "d"))
    , R.inlet "e" -- WithDefault "e" (Num' (nodeId <> "e") 3.0)
    ]
    [ R.outlet "c"
    , R.outlet' "x" $ flow $ map (Num' (nodeId <> "x")) $ fold (\_ n -> n + 1) (interval 5000) 0
    , R.outlet' "y" $ flow $ map (Num' (nodeId <> "y")) $ fold (\_ n -> n + 1) (interval 2000) 0
    ]
    -- (\_ -> [ "c" /\ Int' 10 ] )


myNetwork :: R.Network MyData
myNetwork =
  R.network
    [ R.patch "Patch One"
      [ myNode "1"
      , R.processWith processF $ myNode "2"
      ] -- >>> connect (patch.getNode 0) "a" (patch.getNode 1) "b"
    ]
  where
    processF inputs | Map.isEmpty inputs = Map.empty
    processF inputs | Map.member "d" inputs =
      Map.singleton "c" $ fromMaybe Bang $ Map.lookup "d" inputs
    processF inputs = Map.empty

main :: ∀ e. R.RpdEff ( dom :: DOM, console :: CONSOLE | e ) Unit
main = do
  documentType <- document =<< window
  element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  for_ element (\element ->
    RenderH.renderer element myNetwork
    -- let renderer = RenderH.renderer element
    -- in R.run renderer myNetwork
  )


instance showMyData :: Show MyData where
  show Bang = "Bang"
  show (Str' label s) = "Str: " <> label <> "/" <> s
  show (Num' label n) = "Num: " <> label <> "/" <> show n
