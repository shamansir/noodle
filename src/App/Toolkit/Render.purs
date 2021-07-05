module App.Toolkit.Render where


import Noodle.Node.Unit (Node)


import Halogen.HTML as HH
import Halogen.Svg.Elements as HS


render :: forall w i. String -> Node Int -> HH.HTML w i-- Halogen Component ?
render "sum" node = HS.g [] []
render _ node = HS.g [] []
