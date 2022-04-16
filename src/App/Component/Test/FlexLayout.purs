module App.Component.Test.FlexLayout where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Type.Row (type (+))

import Color as C
import Color.Extra as C

import App.Layout.Flex (Flex, Flex2, Rule)
import App.Layout.Flex as Flex
import App.Layout.Flex.Build as Flex

import Data.Array ((:))
import Data.Array as Array

import Data.Int (toNumber, floor)
import Data.Number
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Tuple (uncurry)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec2 (Vec2, Pos, Size, (<+>))
import Data.Vec2 as V2
import Data.Foldable (foldr)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import App.Svg.Extra as HSA
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS


type Slot id = forall query. H.Slot query Void id


type ColoredBlock = C.Color


type ColoredFlex = Flex2 Rule ColoredBlock
--type ColoredFlex = Flex Size ColoredBlock


type State =
    { colored :: Array (String /\ ColoredFlex)
    , node :: Flex2 Rule String
    }


data Action
    = Initialize


initialState :: State
initialState =
    { colored :
        [ "vert [ fill /\\ horz [ fill /\\ black ] ]" /\

            Flex.vert
                [ Flex.fill /\
                    Flex.horz [ Flex.fill /\ C.black ]
                ]
        , "vert [ fill /\\ horz [ fill /\\ red, fill /\\ green, fill /\\ blue ] ]" /\
            Flex.vert
                [ Flex.fill /\
                    Flex.horz
                        [ Flex.fill /\ C.rgb 255 0 0
                        , Flex.fill /\ C.rgb 0 255 0
                        , Flex.fill /\ C.rgb 0 0 255
                        ]
                ]
        , "vert [ fill /\\ horz [ percent 10 /\\ red, fill /\\ green, fill /\\ blue ] ]" /\
            Flex.vert
                [ Flex.fill /\
                    Flex.horz
                        [ Flex.percents 10 /\ C.rgb 255 0 0
                        , Flex.fill /\ C.rgb 0 255 0
                        , Flex.fill /\ C.rgb 0 0 255
                        ]
                ]
        , "vert [ fill /\\ horz [ percent 15 /\\ red, percent 40 /\\ green ] ]" /\
            Flex.vert
                [ Flex.fill /\
                    Flex.horz
                        [ Flex.percents 15 /\ C.rgb 255 0 0
                        , Flex.percents 40 /\ C.rgb 0 255 0
                        ]
                ]
        , "vert [ fill /\\ horz [ units 55 /\\ red, fill /\\ green, units 120 /\\ blue ] ]" /\
            Flex.vert
                [ Flex.fill /\
                    Flex.horz
                        [ Flex.units 55.0 /\ C.rgb 255 0 0
                        , Flex.fill /\ C.rgb 0 255 0
                        , Flex.units 120.0 /\ C.rgb 0 0 255
                        ]
                ]
        , "vert [ percent 15 /\\ horz [ fill /\\ red ], fill /\\ horz [ fill /\\ green ], percent 40 /\\ [ fill /\\ blue ] ]" /\
            Flex.vert
                [ Flex.percents 15 /\ Flex.horz [ Flex.fill /\ C.rgb 255 0 0 ]
                , Flex.fill /\ Flex.horz [ Flex.fill /\ C.rgb 0 255 0 ]
                , Flex.percents 40 /\ Flex.horz [ Flex.fill /\ C.rgb 0 0 255 ]
                ]
        , "vert [ horz [ portion 1 /\\ red, portion 3 /\\ green, portion 2 /\\ blue ] ]" /\
            Flex.vert
                [ Flex.fill /\
                    Flex.horz
                        [ Flex.portion 1 /\ C.rgb 255 0 0
                        , Flex.portion 3 /\ C.rgb 0 255 0
                        , Flex.portion 2 /\ C.rgb 0 0 255
                        ]
                ]
        ]
    , node :
        Flex.vert
            [ Flex.fill /\
                Flex.horz []
            ]
    }


renderFlex
    :: forall a m slots
     . String
    -> Pos
    -> Size
    -> (Pos -> Size -> a -> H.ComponentHTML Action slots m)
    -> Flex2 Rule a
    -> H.ComponentHTML Action slots m
renderFlex description pos size drawF flex =
    HS.g
        [ HSA.translateTo' pos ]
        [ HS.g
            []
            $ Flex.fold2 (\pos size val prev -> drawF pos size val : prev) []
            $ Flex.fit2 size flex
        , HS.text
                [ HSA.fill $ Just $ C.toSvg $ C.white
                , HSA.font_size $ HSA.FontSizeLength $ HSA.Px 8.0
                , HSA.translateTo' $ V2.w size / 2.0 <+> V2.h size / 2.0
                , HSA.text_anchor $ HSA.AnchorMiddle
                ]
                [ HH.text description
                ]
        ]


renderColoredFlex :: forall m slots. Int -> String -> ColoredFlex -> H.ComponentHTML Action slots m
renderColoredFlex n description =
    renderFlex description (0.0 <+> toNumber n * V2.h size) size drawBox
    where
        size = 500.0 <+> 60.0
        drawBox pos size color =
            HS.g
                [ HSA.translateTo' pos
                ]
                [ HS.rect
                    [ HSA.width $ V2.w size
                    , HSA.height $ V2.h size
                    , HSA.fill $ Just $ C.toSvg color
                    ]
                ]



render
    :: forall m slots
     . MonadEffect m
    => State
    -> H.ComponentHTML Action slots m
render state =
    HS.g
        []
        $ Array.mapWithIndex (uncurry <<< renderColoredFlex) state.colored


handleAction
    :: forall slots output m
     . Action
    -> H.HalogenM State Action slots output m Unit
handleAction = case _ of

    Initialize -> do
        pure unit


component
    :: forall query input output m
     . MonadEffect m
    => H.Component query input output m
component =
    H.mkComponent
        { initialState : const initialState
        , render
        , eval:
            H.mkEval H.defaultEval
                { handleAction = handleAction
                , initialize = Just Initialize
                }
        }
