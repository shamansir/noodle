module Web.Components.MainScreen where

import Prelude

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Control.Monad.State (put, modify) as State

import Data.Maybe (Maybe(..))
import Data.Map (toUnfoldable) as Map
import Data.Tuple.Nested ((/\), type (/\))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Elements as HS

import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit
import Noodle.Network (addPatch, patches) as Network
import Noodle.Patch (make, id, name) as Patch

import Web.State (State)
import Web.State (empty) as CState
import Web.Components.PatchesBar as PatchesBar

type Slots = ( patchesBar :: forall q. H.Slot q Void Unit )

_patchesBar = Proxy :: _ "patchesBar"

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

render :: forall tk ps fs sr cr mi m. State _ tk ps fs sr cr mi -> H.ComponentHTML Action Slots m
render state =
     HH.div_
        [ HS.svg [ HSA.width 1000.0, HSA.height 1000.0 ]
            [ HS.g
                []
                [ HS.rect
                    [ HSA.width 1000.0, HSA.height 1000.0
                    , HSA.fill $ Just $ HC.RGB 16 15 15
                    , HSA.stroke $ Just $ HC.RGB 100 100 100
                    ]
                , HH.slot_ _patchesBar unit PatchesBar.component $
                    { patches : map Patch.name <$> (Map.toUnfoldable $ Network.patches state.network)
                    , selected : Nothing
                    }
                ]
            ]
        ]


handleAction :: forall output tk ps fs sr cr mi m. MonadEffect m => ps -> Action -> H.HalogenM (State _ tk ps fs sr cr mi) Action Slots output m Unit
handleAction pstate = case _ of
    Initialize -> do
        firstPatch <- H.lift $ Patch.make "Patch 1" pstate
        nextState <- State.modify $ \s -> s { network = s.network # Network.addPatch firstPatch }
        State.put nextState