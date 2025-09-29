module Web.Components.RawHtml where

-- From: https://github.com/naglalakk/purescript-halogen-rawhtml


import Prelude

import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Control.Monad.Extra          (whenJust)
import Effect                       (Effect)
import Effect.Class                 (class MonadEffect, liftEffect)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.HTML.Properties      as HP
import Web.HTML                     (HTMLElement)

foreign import setHTML :: HTMLElement -> String -> Effect Unit

type State =
  { html :: String
  , elRef :: H.RefLabel
  }

type Input =
  { html :: String
  , elRef :: H.RefLabel
  }

data Action
  = Initialize
  | Receive Input

component :: forall m
           . MonadEffect m
           --H.Component query Input Output m
          => H.Component (Const Void) Input Void m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive  = Just <<< Receive
      }
    }
  where
  initialState :: Input -> State
  initialState { html, elRef } =
    { html: html
    , elRef: elRef
    }

  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    Initialize -> do
      injectHTML

    Receive input -> do
      H.modify_ _ { html = input.html
                  , elRef = input.elRef
                  }
      injectHTML

  injectHTML :: H.HalogenM State Action () Void m Unit
  injectHTML = do
      state <- H.get
      mbTargetElement <- H.getHTMLElementRef state.elRef
      whenJust mbTargetElement $ liftEffect <<< flip setHTML state.html

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.ref state.elRef ]
      []
