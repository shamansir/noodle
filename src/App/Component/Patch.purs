module App.Component.Patch where


import Prelude

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.List as List
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe as Maybe
import Effect.Class (class MonadEffect, liftEffect)

import Data.BinPack.R2 (Bin2)
import Data.BinPack.R2 as R2

import Noodle.Patch (Patch) as Noodle
import Noodle.Patch as Patch
import Noodle.Toolkit (Toolkit) as Noodle
import Noodle.Toolkit as Toolkit

import App.Colors as Colors
import App.ClassNames as CS
import App.Component.Node as NodeC

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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
    , layout :: Bin2 Number String
    }


data Action d
    = Receive (Input d)
    | AddNode String


initialState :: forall d. Input d -> State d
initialState { patch, toolkit } =
    { patch, toolkit, layout : R2.container 2000.0 2000.0 }


render :: forall d m. State d -> H.ComponentHTML (Action d) Slots m
render { patch, toolkit, layout } =
    HS.g
        []
        [ nodeButtons
        , nodes'
        ]
    where
        tabHeight = 20.0
        tabLength = 60.0
        packedNodes
            = List.catMaybes
            $ (\(name /\ x /\ y /\ w /\ h) ->
                patch
                     #  Patch.findNode name
                    <#> { name, node : _, x, y, w, h }
            ) <$> R2.toList layout
        nodeButtons = HS.g [ HSA.classes CS.nodesTabs ] $ nodeButton <$> (Set.toUnfoldable $ Toolkit.nodeNames toolkit)
        nodeButton name =
            HS.g
                [ HSA.classes CS.nodeButton
                , HE.onClick \_ -> AddNode name
                ]
                [ HS.rect [ HSA.width tabLength, HSA.height tabHeight, HSA.fill $ Just Colors.tabBackground ]
                , HS.text [] [ HH.text name ]
                ]
        node' idx { node, name, x, y, w, h } =
            HS.g
                [ HSA.transform [ HSA.Translate x $ tabHeight + y ] ]
                [ HH.slot _node idx NodeC.component { node, name } absurd ]
        nodes' = HS.g [ HSA.classes CS.nodes ] $ Array.mapWithIndex node' $ List.toUnfoldable $ packedNodes -- Patch.nodes patch


handleAction :: forall output m d. MonadEffect m => Action d -> H.HalogenM (State d) (Action d) Slots output m Unit
handleAction = case _ of

  Receive _ ->
    pure unit
    --H.modify_ (\state -> state { patch = input.patch })

  AddNode name -> do
    toolkit <- H.gets _.toolkit
    case Toolkit.spawn name toolkit of
        Just newNode' -> do
            newNode <- liftEffect newNode'
            H.modify_ -- _ { patch = _.patch # Patch.addNode "sum" newNode }
                (\state ->
                    let nodeName = makeUniqueName state.patch name
                        --inletsCount /\ outletsCount = Node.dimensions newNode'
                    in state
                        { patch = state.patch # Patch.addNode nodeName newNode
                        , layout = R2.packOne state.layout (R2.item 30.0 40.0 nodeName)
                                    # Maybe.fromMaybe state.layout
                        }
                )
        Nothing -> pure unit
    where makeUniqueName patch name = name <> "-" <> (show $ Patch.nodesCount patch + 1)


component :: forall query output m d. MonadEffect m => H.Component query (Input d) output m
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