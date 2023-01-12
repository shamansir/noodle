module Web.LayoutRenderer where

-- TODO: WebApp.Layout.Render
-- TODO: WebApp.Layout.Instance.Flex
-- TODO: WebApp.Layout.Instance....

import Prelude

import Color as C
import Color.Extra (toSvg, transparent) as C

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Vec2 (Size, Pos, (<+>))
import Data.Vec2 as V2
import Web.Layout (class IsLayout)
import Web.Layout as Layout
import Web.Svg.Extra as HSA

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA



render :: forall l a trg. IsLayout l => (a -> Pos -> Size -> trg) -> l a -> Array trg
render f = Layout.fold ((:) <<< \(a /\ pos /\ size) -> f a pos size) []


renderToSvg
    :: forall p i l a
     . IsLayout l
    => (a -> Pos -> Size -> HH.HTML p i)
    -> l a
    -> HH.HTML p i
renderToSvg renderItem =
    HS.g [] <<< render rectAt
    where
        rectAt a pos size =
            HS.g
                [ HSA.translateTo' pos ]
                [ renderItem a pos size ]


renderToSvgText
    :: forall p i l a
     . IsLayout l
    => Show a
    => l a
    -> HH.HTML p i
renderToSvgText =
    -- through `renderToSvg`
    HS.g [] <<< render rectAt
    where
        rectAt a pos size =
            HS.g
                []
                [ HS.rect
                    [ HSA.x $ V2.x pos, HSA.y $ V2.y pos
                    , HSA.width $ V2.w size, HSA.height $ V2.h size
                    , HSA.fill $ Just $ C.toSvg C.transparent
                    , HSA.stroke $ Just $ C.toSvg C.black
                    , HSA.strokeWidth 1.0
                    ]
                , HS.text
                    [ HSA.translateTo' $ pos + ((V2.w size / 2.0) <+> (V2.h size / 2.0))
                    , HSA.text_anchor $ HSA.AnchorMiddle
                    ]
                    [ HH.text $ show a ]
                ]