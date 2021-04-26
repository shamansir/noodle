module RayDraw.Toolkit.Render.Html where


import Data.Array
import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Number (fromString)
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
import RayDraw.Toolkit.Value (Product(..), RayPoints, RgbaColor(..), Value(..), allProducts, colorToCss, getColor1, getColor2, getColor3, getPalette, getRayPoints, productShortName, rayPoints)
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
          (renderPaletteInput path <$> allProducts)                
        ]

renderNode RayPointsNode (R.Node _ path _ _ _) atInlet _ = 
    let 
      rayPointsInput = case atInlet "points" of 
            (Just (Points points)) -> points
            _ -> rayPoints []
      pointsArray = getRayPoints rayPointsInput
      updateXValue index point val = Points (rayPoints (fromMaybe [] $ updateAt index {x:val, y:point.y} pointsArray))
      updateYValue index point val = Points (rayPoints (fromMaybe [] $ updateAt index {x:point.x, y: val} pointsArray))
      inputRow index point = H.div [H.classes ["tk-points-row"]] [           
                H.input [H.type_ H.InputNumber, 
                         H.value $ show point.x,
                         H.onValueChange (\v ->
                                    fromString v
                                    <#> toInlet path "points" <<< updateXValue index point) 
                        ],
                H.input [H.type_ H.InputNumber, 
                         H.value $ show point.y,
                         H.onValueChange (\v ->
                                    fromString v
                                    <#> toInlet path "points" <<< updateYValue index point) ]
            ]
    in
    H.div
        [ H.classes [ "tk-node" ] ]        
        (mapWithIndex inputRow pointsArray)

renderNode _ _ _ _ =
    H.div
        [ H.classes [ "tk-node" ] ]
        [ ]

renderPaletteInput :: Path.ToNode -> Product -> View
renderPaletteInput path prod = 
    let 
      palette = getPalette prod 
      shortName = productShortName prod
      color1 = getColor1 palette
      color2 = getColor2 palette
      color3 = getColor3 palette
      colorDiv col = H.div [H.classes ["tk-palette-color"], H.styles $ [H.Style "background-color" $ colorToCss col]] []
    in
    H.label [H.classes [ "tk-palette-item" ]] 
        [ H.div [H.classes ["tk-palette-name"]] [H.text shortName],
        H.input [H.type_ H.InputRadio, 
                 H.name "palette",
                 H.onClick $ H.always_ $ toOutlet path "palette" $ Palette palette
                     ],
        H.div [H.classes ["tk-palette-checkmark"]] [
            colorDiv color1,
            colorDiv color2,
            colorDiv color3
        ] 
        ]

colorPickerTile :: RgbaColor -> Path.ToNode -> View
colorPickerTile color path = H.span [    
                     H.classes ["tk-color-value"],
                     H.styles $ [H.Style "background-color" $ colorToCss color],
                     H.onClick $ H.always_ $ toOutlet path "color" $ Color color
                   ]  
                []