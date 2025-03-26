module Web.Components.NodeBox where

import Prelude


import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array ((:))
import Data.Array (length, snoc) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (length, toUpper) as String
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Id (FamilyR, family, familyOf, inletRName, outletRName) as Id
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, shape) as RawNode
import Noodle.Raw.Fn.Shape as RawShape

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type Input sterpr chrepr m =
    { node :: Raw.Node sterpr chrepr m
    }


type State sterpr chrepr m =
    { node :: Raw.Node sterpr chrepr m
    }


data Action sterpr chrepr m
    = Initialize
    | Receive (Input sterpr chrepr m)


data Output
    = Output


component :: forall query m sterpr chrepr mi. H.Component query (Input sterpr chrepr mi) Output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }


initialState :: forall sterpr chrepr mi. Input sterpr chrepr mi -> State sterpr chrepr mi
initialState { node } = { node }


render :: forall m sterpr chrepr mi. State sterpr chrepr mi -> H.ComponentHTML (Action sterpr chrepr mi) () m
render { node } =
    HS.g
        [ HSA.transform [ HSA.Translate 200.0 80.0 ] ]
        (
            HS.path
                [ HSA.transform [ HSA.Translate (-2.0) 0.0 ]
                , HSA.d
                    [ HSA.m HSA.Abs titleBarWidth channelBarHeight
                    , HSA.l HSA.Abs 0.0 $ channelBarHeight + slopeFactor
                    , HSA.l HSA.Abs 0.0 $ titleY - slopeFactor
                    , HSA.l HSA.Abs titleBarWidth titleY
                    , HSA.z
                    ]
                , HSA.fill $ Just $ P.hColorOf $ _.i900 Palette.yellow
                , HSA.stroke $ Just $ P.hColorOf $ _.i100 Palette.yellow
                , HSA.strokeWidth 1.5
                ]
            : HS.path
                [ HSA.transform [ HSA.Translate titleBarWidth channelBarHeight ]
                , HSA.d
                    [ HSA.m HSA.Abs 0.0 (slopeFactor * 2.0)
                    , HSA.l HSA.Abs slopeFactor slopeFactor
                    , HSA.l HSA.Abs (nodeWidth * 0.7) slopeFactor
                    , HSA.l HSA.Abs (nodeWidth * 0.7 + slopeFactor) 0.0
                    , HSA.l HSA.Abs (nodeWidth - slopeFactor) 0.0
                    , HSA.l HSA.Abs nodeWidth slopeFactor
                    , HSA.l HSA.Abs nodeWidth (bodyHeight - slopeFactor)
                    , HSA.l HSA.Abs (nodeWidth - slopeFactor) bodyHeight
                    , HSA.l HSA.Abs slopeFactor bodyHeight
                    , HSA.l HSA.Abs 0.0 (bodyHeight - slopeFactor)
                    , HSA.z
                    ]
                , HSA.fill $ Just $ P.hColorOf $ _.i950 Palette.base_
                ]
            : HS.text
                [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.yellow
                , HSA.font_size $ HSA.FontSizeLength $ HSA.Px titleFontSize
                , HSA.dominant_baseline HSA.Hanging
                , HSA.transform
                    [ HSA.Translate 0.0 titleY
                    , HSA.Translate 0.0 (-slopeFactor - 2.0)
                    , HSA.Rotate 270.0 0.0 0.0
                    ]
                ]
                [ HH.text $ Id.family $ Id.familyOf $ RawNode.id node ]
            : renderInlets
            : renderOutlets
            : []
        )
    where
        inletsDefs = RawShape.inlets $ RawNode.shape node
        outletsDefs = RawShape.outlets $ RawNode.shape node
        inletsCount = Array.length inletsDefs
        outletsCount = Array.length outletsDefs
        channelStep = 55.0
        channelBarHeight = 15.0
        channelBarWidth = nodeWidth - titleWidth
        bodyHeight = 70.0
        titleWidth = 20.0
        titleBarWidth = titleWidth - slopeFactor
        channelFontSize = 9.0
        titleFontSize = 11.0
        connectorRadius = 5.0
        maxChannelsCount = max inletsCount outletsCount
        nodeWidth = titleWidth + (channelStep * Int.toNumber maxChannelsCount)
        titleY = channelBarHeight + bodyHeight
        channelNameShift = connectorRadius + 4.0
        -- fullHeight = channelBarHeight + bodyHeight + channelBarHeight
        slopeFactor = 5.0
        renderInlet idx inletDef =
            HS.g
                [ HSA.transform [ HSA.Translate (Int.toNumber idx * channelStep) 0.0 ] ]
                [ HS.circle
                    [ HSA.fill $ Just $ P.hColorOf $ _.i200 Palette.blue
                    , HSA.r connectorRadius
                    , HSA.cy $ connectorRadius / 2.0 + 2.0 -- channelBarHeight / 2.0
                    ]
                , HS.text
                    [ HSA.x channelNameShift
                    , HSA.fill $ Just $ P.hColorOf $ _.i50 Palette.blue
                    , HSA.dominant_baseline HSA.Hanging
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px channelFontSize
                    ]
                    [ HH.text $ String.toUpper $ Id.inletRName inletDef.name ]
                ]
        renderOulet idx outletDef =
            HS.g
                [ HSA.transform [ HSA.Translate (Int.toNumber idx * channelStep) 0.0 ] ] -- TODO reverse order so that outlets align to the right side, or even better to bottom right corner
                [ HS.circle
                    [ HSA.fill $ Just $ P.hColorOf $ _.i200 Palette.blue
                    , HSA.r connectorRadius
                    , HSA.cy $ connectorRadius + 2.0 -- channelBarHeight / 2.0
                    ]
                , HS.text
                    [ HSA.x channelNameShift
                    , HSA.y 3.0
                    , HSA.fill $ Just $ P.hColorOf $ _.i50 Palette.blue
                    , HSA.dominant_baseline HSA.Hanging
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px channelFontSize
                    ]
                    [ HH.text $ String.toUpper $ Id.outletRName outletDef.name ]
                ]
        renderInlets =
            HS.g
                [ HSA.transform [ HSA.Translate titleWidth 0.0 ] ]
                (
                    HS.path
                        [ HSA.d
                            [ HSA.m HSA.Abs 0.0 0.0
                            , HSA.l HSA.Abs channelBarWidth 0.0
                            , HSA.l HSA.Abs channelBarWidth slopeFactor
                            , HSA.l HSA.Abs (channelBarWidth + slopeFactor) slopeFactor
                            , HSA.l HSA.Abs (channelBarWidth + slopeFactor) channelBarHeight
                            , HSA.l HSA.Abs channelBarWidth (channelBarHeight + slopeFactor)
                            , HSA.l HSA.Abs channelBarWidth channelBarHeight
                            , HSA.l HSA.Abs 0.0 channelBarHeight
                            , HSA.l HSA.Abs 0.0 (channelBarHeight + slopeFactor)
                            , HSA.l HSA.Abs (-slopeFactor) channelBarHeight
                            , HSA.l HSA.Abs (-slopeFactor) slopeFactor
                            , HSA.l HSA.Abs 0.0 slopeFactor
                            , HSA.z
                            ]
                        , HSA.fill $ Just $ P.hColorOf $ _.i900 Palette.blue
                        ]
                    : [ HS.g
                        [ HSA.transform [ HSA.Translate 3.0 3.0 ] ]
                        $ mapWithIndex renderInlet inletsDefs ]
                )
        renderOutlets =
            HS.g
                [ HSA.transform [ HSA.Translate titleWidth $ channelBarHeight + bodyHeight ] ]
                (
                    HS.path
                        [ HSA.d
                            [ HSA.m HSA.Abs 0.0 (-slopeFactor)
                            , HSA.l HSA.Abs 0.0 0.0
                            , HSA.l HSA.Abs channelBarWidth 0.0
                            , HSA.l HSA.Abs channelBarWidth (-slopeFactor)
                            , HSA.l HSA.Abs (channelBarWidth + slopeFactor) 0.0
                            , HSA.l HSA.Abs (channelBarWidth + slopeFactor) (channelBarHeight - slopeFactor)
                            , HSA.l HSA.Abs channelBarWidth (channelBarHeight - slopeFactor)
                            , HSA.l HSA.Abs channelBarWidth channelBarHeight
                            , HSA.l HSA.Abs 0.0 channelBarHeight
                            , HSA.l HSA.Abs (-slopeFactor) (channelBarHeight - slopeFactor)
                            , HSA.l HSA.Abs (-slopeFactor) 0.0
                            , HSA.z
                            ]
                        , HSA.fill $ Just $ P.hColorOf $ _.i900 Palette.blue
                        ]
                    : [ HS.g
                        [ HSA.transform [ HSA.Translate 3.0 0.0 ] ]
                        $ mapWithIndex renderOulet outletsDefs ]
                )



handleAction :: forall m sterpr chrepr mi. Action sterpr chrepr mi -> H.HalogenM (State sterpr chrepr mi) (Action sterpr chrepr mi) () Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    Receive input ->
        H.modify_ _
            { node = input.node
            }
