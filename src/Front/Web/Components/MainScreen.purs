module Front.Web.Components.MainScreen where

import Prelude


import Effect (Effect)
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS

import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit

import Front.Web.State (State)
import Front.Web.State (empty, init) as State

data Action
    = Start

component :: forall query input output tk fs sr cr mi m. MonadEffect mi => Toolkit tk fs sr cr mi -> H.Component query input output m
component toolkit =
  H.mkComponent
    { initialState : initialState toolkit
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input tk ps fs sr cr m. MonadEffect m => Toolkit tk fs sr cr m -> input -> State _ tk ps fs sr cr m
initialState toolkit _ = State.empty toolkit

render :: forall tk ps fs sr cr mi mo. State _ tk ps fs sr cr mi -> H.ComponentHTML Action () mo
render state =
  HH.div_
    [ HH.text "Test"
    , HS.svg [ HSA.width 100.0, HSA.height 100.0 ] []
    ]

handleAction :: forall output tk ps fs sr cr mi m. Action -> H.HalogenM (State _ tk ps fs sr cr mi) Action () output m Unit
handleAction = case _ of
  Start -> pure unit