module Hydra.Component.Node.Pi where


import Prelude (const, ($), Unit, unit)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (Maybe(..))

import App.Toolkit.UI (FromNode(..)) as UI

import Hydra as Hydra
import Hydra.Toolkit.UI.Components (NodeComponent, NodeOutput) as UI

import Halogen as H
import Halogen.Svg.Elements as HS
import Halogen.HTML.Events as HE
import Halogen.HTML as HH


data Action
    = Send


render :: forall m. Unit -> H.ComponentHTML Action () m
render _ =
    HS.g
        [ ]
        [ HS.text [ HE.onClick $ const $ Send ] [ HH.text "PI" ]
        ]


handleAction :: forall m. Action -> H.HalogenM Unit Action () UI.NodeOutput m Unit
handleAction = case _ of
    Send ->
        H.raise $ UI.SendToOutlet "pi" $ Hydra.pi


component :: forall m. UI.NodeComponent m
component =
    H.mkComponent
        { initialState : const unit
        , render
        , eval: H.mkEval H.defaultEval
            { initialize = Just Send
            , handleAction = handleAction
            }
        }