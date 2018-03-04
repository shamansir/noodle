module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C

import Rpd as Rpd
import Signal.Channel as SC

import Data.Foldable (for_)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..))
import Text.Smolder.Renderer.DOM (render)

import Text.Smolder.Markup (Markup, text) as H
import Text.Smolder.HTML (div) as H


view :: ∀ e. H.Markup e
view =
  H.div
    $ H.text "Hello, PoieScript!"


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


main :: ∀ e. Eff (dom :: DOM | e) Unit
main = do
  documentType <- document =<< window
  element <- getElementById (ElementId "app") $ htmlDocumentToNonElementParentNode documentType
  for_ element (render <@> view)
