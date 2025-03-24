module Front.Web.Components.MainScreen where

import Prelude


import Effect (Effect)
import Effect.Class (class MonadEffect)

import Control.Monad.State (put, modify) as State

import Data.Maybe (Maybe(..))
import Halogen.Svg.Attributes.Color as C

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
import Noodle.Network (addPatch) as Network
import Noodle.Patch (make) as Patch

import Front.Web.State (State)
import Front.Web.State (empty) as CState

data Action
    = Initialize

component :: forall query input output ps tk fs sr cr mi m. MonadEffect m => ps -> Toolkit tk fs sr cr mi -> H.Component query input output m
component pstate toolkit =
  H.mkComponent
    { initialState : initialState toolkit
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction pstate
        , initialize = Just Initialize
        }
    }

initialState :: forall input tk ps fs sr cr mi. Toolkit tk fs sr cr mi -> input -> State _ tk ps fs sr cr mi
initialState toolkit _ = CState.empty toolkit

render :: forall tk ps fs sr cr mi m. State _ tk ps fs sr cr mi -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HS.svg [ HSA.width 1000.0, HSA.height 1000.0 ] [
        HS.rect
            [ HSA.width 1000.0, HSA.height 1000.0
            , HSA.fill $ Just $ C.RGB 16 15 15
            , HSA.stroke $ Just $ C.RGB 100 100 100
            ]
        ]
    ]

handleAction :: forall output tk ps fs sr cr mi m. MonadEffect m => ps -> Action -> H.HalogenM (State _ tk ps fs sr cr mi) Action () output m Unit
handleAction pstate = case _ of
  Initialize -> do
    firstPatch <- H.lift $ Patch.make "Patch 1" pstate
    nextState <- State.modify $ \s -> s { network = s.network # Network.addPatch firstPatch }
    State.put nextState