module App.LayoutRenderer where

import Prelude

import Data.Array ((:))
import Data.Tuple.Nested ((/\))
import Data.Vec2 (Size, Pos)
import Data.Vec2 as V2
import App.Layout (class IsLayout)
import App.Layout as Layout

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA



render :: forall l a trg. IsLayout l => (a -> Pos -> Size -> trg) -> l a -> Array trg
render f = Layout.fold ((:) <<< \(a /\ pos /\ size) -> f a pos size) []


renderToSvgText
    :: forall action slots m l a
     . IsLayout l
    => Show a
    => l a
    -> H.ComponentHTML action slots m
renderToSvgText =
    HS.g [] <<< render rectAt
    where
        rectAt a pos size =
            HS.g
                []
                [ HS.rect [ HSA.x $ V2.x pos, HSA.y $ V2.y pos, HSA.width $ V2.w size, HSA.height $ V2.h size ]
                , HS.text [] [ HH.text $ show a ]
                ]