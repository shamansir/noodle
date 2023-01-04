module App.Component.App3 where

import Prelude

import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML.Core as HH
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS

import Noodle.Network2 (Network) as Noodle
import Noodle.Toolkit3 (Toolkit) as Noodle
import Noodle.Patch4 as Patch


type Slots =
    ( )


type Input gstate nodes instances =
    { network :: Noodle.Network gstate nodes instances
    , toolkit :: Noodle.Toolkit gstate nodes
    , currentPatch :: Maybe Patch.Id
    , patchState :: gstate
    }


type State gstate nodes instances =
    { network :: Noodle.Network gstate nodes instances
    , toolkit :: Noodle.Toolkit gstate nodes
    , currentPatch :: Maybe Patch.Id
    , patchState :: gstate
    }


data Action
    = Initialize


initialState
    :: forall patch_state node_state d
     . Input patch_state node_state d
    -> State patch_state node_state d
initialState { network, toolkit, currentPatch, patchState } =
    { network, toolkit, patchState
    , currentPatch
    }


render
    :: forall patch_action gstate nodes instances rln rli rla
     . State gstate nodes instances
    -> H.ComponentHTML Action Slots Aff -- FIXME: there is MonadAff here!
render state = HH.div [] []



handleAction
    :: forall output patch_action rl gstate (nodes :: Row Type) (instances :: Row Type) m
     . MonadAff m
    => MonadEffect m
    => Action
    -> H.HalogenM (State gstate nodes instances) Action Slots output m Unit
handleAction = case _ of
    Initialize -> do
        pure unit


component
    :: forall query output gstate nodes instances rln rli rla
     . H.Component query (Input gstate nodes instances) output Aff -- FIXME: there is MonadAff here!
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }