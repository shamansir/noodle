module RayDraw.Toolkit.Render.Html where


import Data.Array
import Prelude

import Data.Maybe (maybe)
import Noodle.API.Action (Action(..), DataAction(..), RequestAction(..)) as A
import Noodle.Network (Node(..)) as R
import Noodle.Path as P
import Noodle.Path as Path
import Noodle.Process (Receive, Send) as R
import Noodle.Render.Html (ToolkitRenderer, core) as R
import Noodle.Render.Html.NodeList (render) as NodeList
import RayDraw.Toolkit.Channel (Channel(..))
import RayDraw.Toolkit.Node (Node(..), nodesForTheList)
import RayDraw.Toolkit.Render.Html.ToHtml (View, toInlet, toOutlet)
import RayDraw.Toolkit.Value (RgbaColor(..), Value(..), colorToCss)
import Spork.Html (Html, IProp, InputType)
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

renderNode ColorNode (R.Node _ path _ _ _) _ _ = 
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

renderNode ProductPaletteNode (R.Node _ path _ _ _) _ _ = 
    H.div
        [ H.classes [ "tk-node" ] ]
        [ H.div [] 
          (const renderPaletteInput <$> 1 .. 9)                 
        ]

renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]

renderPaletteInput :: View
renderPaletteInput = H.label [H.classes [ "tk-palette-item" ]] 
    [ H.div [H.classes ["tk-palette-name"]] [H.text "JB"],
      H.input [H.type_ H.InputRadio, H.name "palette"],
      H.div [H.classes ["tk-palette-checkmark"]] [
         H.div [H.classes ["tk-palette-color"]] [],
         H.div [H.classes ["tk-palette-color"]] [],
         H.div [H.classes ["tk-palette-color"]] []    
      ] 
    ]

colorPickerTile :: RgbaColor -> Path.ToNode -> View
colorPickerTile color path = H.span [    
                     H.classes ["tk-color-value"],
                     H.styles $ [H.Style "background-color" $ colorToCss color],
                     H.onClick $ H.always_ $ toOutlet path "color" $ Color color
                   ]  
                []