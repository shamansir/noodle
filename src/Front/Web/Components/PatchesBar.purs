module Web.Components.PatchesBar where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Control.Monad.State (modify, put) as State

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

import Noodle.Id (PatchR, PatchName) as Id
import Noodle.Patch (Patch)
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit
import Noodle.Network (addPatch) as Network


type Input =
    { patches :: Array (Id.PatchR /\ Id.PatchName)
    , selected :: Maybe Id.PatchR
    }


type State =
    { patches :: Array (Id.PatchR /\ Id.PatchName)
    , selected :: Maybe Id.PatchR
    }


data Action
    = Initialize
    | Receive Input


component :: forall query output m. H.Component query Input output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }

initialState :: Input -> State
initialState { patches, selected } = { patches, selected }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HS.g
    []
    $ patchButton <$> state.patches
    where
        patchButton _ =
            HS.rect
                [ HSA.width 20.0, HSA.height 20.0
                , HSA.fill $ Just $ HC.RGB 150 15 15
                , HSA.stroke $ Just $ HC.RGB 100 100 100
                ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    Initialize -> pure unit
    Receive input ->
        H.modify_ _
            { patches = input.patches
            , selected = input.selected
            }
