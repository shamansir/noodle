module Xodus.Toolkit.Render.Html where


import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.List (toUnfoldable)

import Noodle.Network (Node(..)) as R
import Noodle.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Noodle.Render.Html (View, ToolkitRenderer, core, my) as R
import Noodle.Render.Html.NodeList (render) as NodeList
import Noodle.Path as P
import Noodle.Process (Receive, Send) as R

import Noodle.Render.Atom as R

import Spork.Html (Html)
import Spork.Html as H

import Xodus.Toolkit.Node (Node(..), nodesForTheList)
import Xodus.Toolkit.Value (Value(..))
import Xodus.Toolkit.Channel (Channel(..))
import Xodus.Dto


renderer :: R.ToolkitRenderer Value Channel Node
renderer =
    { renderNode : renderNode
    , renderInlet : \c _ d ->
        H.div
            [ H.classes [ "tk-inlet", classFor c ] ]
            [ H.text $ maybe "?" show d ]
    , renderOutlet : \c _ d ->
        H.div
            [ H.classes [ "tk-outlet", classFor c ] ]
            [ H.text $ maybe "?" show d ]
    }
    where
        classFor Channel = "tk-channel"


renderNode
    :: Node
    -> R.Node Value Node
    -> R.Receive Value
    -> R.Send Value
    -> R.View Value Channel Node

renderNode NodeListNode (R.Node _ (P.ToNode { patch }) _ _ _) _ _ =
    NodeList.render (P.ToPatch patch) nodesForTheList

renderNode ConnectNode (R.Node _ path _ _ _) _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick $ toInlet path "bang" $ Bang ]
            [ H.span [ H.classes [ "xodus-connect-button" ] ] [ H.text "◌" ] ]
        ]

renderNode SourceNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ case atInlet "databases" of
               Just (Databases databases) -> H.ul [] $ renderDatabase <$> toUnfoldable databases
               _ -> H.text "no databases"
        ]
    where
        renderDatabase :: Database -> R.View Value Channel Node
        renderDatabase database@(Database { location }) =
            H.li
                [ H.classes [ "xodus-list-item xodus-database" ]
                , H.onClick $ toInlet path "only" $ SelectDatabase database
                ]
                [ H.text location ]

renderNode AllOfNode (R.Node _ path _ _ _) atInlet _ =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atInlet "source" of
                Just (Source database entityTypes) ->
                    H.div
                        []
                        [ H.span
                            [ H.onClick $ toInlet path "only" $ Bang ]
                            [ H.text "None" ]
                        , H.ul [] $ renderEntityType <$> toUnfoldable entityTypes
                        ]
                _ -> H.text "no entity types"
        ]
    where
        renderEntityType :: EntityType -> R.View Value Channel Node
        renderEntityType entityType@(EntityType { id, name }) =
            H.li
                [ H.classes [ "xodus-list-item xodus-entity-type" ]
                , H.onClick $ toInlet path "only" $ SelectType $ entityType
                ]
                [ H.text $ show id <> ": " <> name ]

renderNode SelectNode _ _ atOutlet =
    H.div
        [ H.classes [ "tk-node" ] ]
            [ case atOutlet "result" of
                Just (Result entities) ->
                    H.ul [] $ renderEntity <$> toUnfoldable entities
                _ -> H.text "no entities"
        ]
    where
        renderEntity :: Entity -> R.View Value Channel Node
        renderEntity (Entity entity) =
            H.li
                [ H.classes [ "xodus-list-item xodus-entity" ]
                ]
                [ H.text $ show entity.id ]


toInlet :: P.ToNode -> P.Alias -> Value -> _
toInlet path alias v =
    H.always_ $ R.core
        $ A.Request
        $ A.ToSendToInlet (P.inletInNode path alias)
        $ v


toOutlet:: P.ToNode -> P.Alias -> Value -> _
toOutlet path alias v =
    H.always_ $ R.core
        $ A.Request
        $ A.ToSendToOutlet (P.outletInNode path alias)
        $ v
