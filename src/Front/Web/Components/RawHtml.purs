module Web.Components.RawHtml where

-- From: https://github.com/naglalakk/purescript-halogen-rawhtml


import Prelude

import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Effect                       (Effect)
import Effect.Class                 (class MonadEffect, liftEffect)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.HTML.Properties      as HP
import Web.HTML                     (HTMLElement)

foreign import setHTML :: HTMLElement -> String -> Effect Unit

type State =
  { html :: String
  , elRef :: String
  }

type Input =
  { html :: String
  , elRef :: String
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
      state <- H.get
      H.getHTMLElementRef (H.RefLabel state.elRef) >>= case _ of
        Nothing -> pure unit
        Just el -> do
          liftEffect $ setHTML el state.html
      pure unit

    Receive input -> H.modify_ _ { html = input.html
                                , elRef = input.elRef
                                }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.ref (H.RefLabel state.elRef) ]
      []
