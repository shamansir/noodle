module App.Component.Patch where


import Prelude

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))

import Noodle.Patch (Patch) as Noodle
import Noodle.Patch as Patch
import Noodle.Toolkit (Toolkit) as Noodle
import Noodle.Toolkit as Toolkit

import App.Colors as Colors
import App.Component.Node as NodeC

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA

import Type.Proxy (Proxy(..))


type Slot id = forall query. H.Slot query Void id


type Slots = ( node :: NodeC.Slot Int )


_node = Proxy :: Proxy "node"


type Input d =
    { patch :: Noodle.Patch d
    , toolkit :: Noodle.Toolkit d
    }


type State d =
    { patch :: Noodle.Patch d
    , toolkit :: Noodle.Toolkit d
    }


data Action d
    = Receive (Input d)
    | AddNode String


initialState :: forall d. Input d -> State d
initialState = identity


render :: forall d m. State d -> H.ComponentHTML (Action d) Slots m
render { patch, toolkit } =
    HS.g
        []
        [ nodeButtons
        , nodes
        ]
    where
        tabHeight = 20.0
        tabLength = 60.0
        nodeButtons = HS.g [ HSA.class_ $ H.ClassName "patches-tabs" ] $ nodeButton <$> (Set.toUnfoldable $ Toolkit.nodeNames toolkit)
        nodeButton name =
            HS.g
                [ HSA.class_ $ H.ClassName "node-button" ]
                [ HS.rect [ HSA.width tabLength, HSA.height tabHeight, HSA.fill $ Just Colors.tabBackground ]
                , HS.text [] [ HH.text name ]
                ]
        node (label /\ n) =
            HS.g
                [ HSA.transform [ HSA.Translate 0.0 tabHeight ] ]
                [ HH.slot _node 0 NodeC.component { node : n } absurd ]
        nodes = HS.g [ HSA.class_ $ H.ClassName "nodes" ] (node <$> Patch.nodes patch)


handleAction :: forall output m d. Action d -> H.HalogenM (State d) (Action d) Slots output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ (\state -> state { patch = input.patch })
  AddNode name ->
    H.modify_ (\state -> state)


component :: forall query output m d. H.Component query (Input d) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval H.defaultEval
                { handleAction = handleAction
                , receive = Just <<< Receive
                }
        }