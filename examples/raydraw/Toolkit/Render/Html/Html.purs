module RayDraw.Toolkit.Render.Html where


import Prelude

import DOM.HTML.Indexed as I
import Data.Either (Either(..), either)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Noodle.API.Action (Action(..), RequestAction(..), DataAction(..)) as A
import Noodle.Network (Node(..)) as R
import Noodle.Path as P
import Noodle.Path as Path
import Noodle.Process (Receive, Send) as R
import Noodle.Render.Atom as R
import Noodle.Render.Html (View, RoutedAction, ToolkitRenderer, core) as R
import Noodle.Render.Html.NodeList (render) as NodeList
import RayDraw.Toolkit.Channel (Channel(..))
import RayDraw.Toolkit.Node (Node(..), nodesForTheList)
import RayDraw.Toolkit.Render.Html.ToHtml (View, toInlet)
import RayDraw.Toolkit.Value (RgbaColor(..), Value(..), colorToCss)
import Spork.Html (Html)
import Spork.Html as H


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
    -> View


renderNode NodeListNode (R.Node _ (P.ToNode { patch }) _ _ _) _ _ =
    NodeList.render (P.ToPatch patch) nodesForTheList

renderNode BangNode (R.Node _ path _ _ _) _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ H.onClick
                $ H.always_ $ R.core
                $ A.Request
                $ A.ToSendToInlet (P.inletInNode path "bang")
                $ Bang
            ]
            [ H.text "◌" ]
        ]

renderNode PaletteNode (R.Node _ path _ _ _) _ _ = 
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div
            [ ]
            [ colorPickerTile (RgbaColor {r : 0.0, g : 1.0, b : 0.0, a : 1.0}) path,
              colorPickerTile (RgbaColor {r : 1.0, g : 1.0, b : 0.0, a : 1.0}) path,
              colorPickerTile (RgbaColor {r : 1.0, g : 0.0, b : 1.0, a : 1.0}) path,
              colorPickerTile (RgbaColor {r : 1.0, g : 1.0, b : 1.0, a : 1.0}) path              
            ]
        ]

renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]


colorPickerTile :: forall i. RgbaColor -> Path.ToNode -> Html i
colorPickerTile color path = H.span [                
                    H.styles $ [H.Style "background-color" $ colorToCss color] <> paletteCellStyle
                   ]  
                []

paletteCellStyle :: Array H.Style
paletteCellStyle = [H.Style "float" "left", 
                    H.Style "width" "10px", 
                    H.Style "height" "10px",  
                    H.Style "display" "block", 
                    H.Style "margin" "1px", 
                    H.Style "border" "1px solid rgba(255,255,255,.1)", 
                    H.Style "border-radius" "2px"]