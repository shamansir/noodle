module Web.Components.NodeBox where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..), maybe)
import Data.Map (lookup) as Map
import Data.Map.Extra (mapKeys) as MapX
import Data.Array ((:))
import Data.Array (length, snoc) as Array
import Data.Tuple (snd) as Tuple
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
import Halogen.Svg.Elements.Extra as HSX

import Noodle.Id (FamilyR, family, familyOf, inletRName, outletRName) as Id
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (NodeChanges, id, shape) as RawNode
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Repr.ChRepr (class WriteChannelRepr, writeChannelRepr)
import Noodle.Repr.ValueInChannel (resolve) as ViC

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type Input strepr chrepr m =
    { node :: Raw.Node strepr chrepr m
    , position :: { left :: Number, top :: Number }
    }


type State strepr chrepr m =
    { node :: Raw.Node strepr chrepr m
    , position :: { left :: Number, top :: Number }
    , beingDragged :: Boolean
    , latestUpdate :: Maybe (RawNode.NodeChanges strepr chrepr)
    }


data Action sterpr chrepr m
    = Initialize
    | Receive (Input sterpr chrepr m)
    | HeaderClick


data Output
    = HeaderWasClicked


data Query strepr chrepr a
    = QueryUpdate (RawNode.NodeChanges strepr chrepr) a
    | QueryDragStart a
    | QueryDragEnd a


component :: forall strepr chrepr m. WriteChannelRepr chrepr => H.Component (Query strepr chrepr) (Input strepr chrepr m) Output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , receive = Just <<< Receive
            }
        }


initialState :: forall sterpr chrepr m. Input sterpr chrepr m -> State sterpr chrepr m
initialState { node, position } =
    { node, position
    , latestUpdate : Nothing
    , beingDragged : false
    }


render :: forall sterpr chrepr m. WriteChannelRepr chrepr => State sterpr chrepr m -> H.ComponentHTML (Action sterpr chrepr m) () m
render { node, position, latestUpdate, beingDragged } =
    HS.g
        [ HSA.transform [ HSA.Translate position.left position.top ]
        ]
        (
            HS.g
                [ HE.onClick $ const $ HeaderClick ]
                [ HS.path
                    [ HSA.transform [ HSA.Translate (-2.0) 0.0 ]
                    , HSA.d
                        [ HSA.m HSA.Abs titleBarWidth channelBarHeight
                        , HSA.l HSA.Abs 0.0 $ channelBarHeight + slopeFactor
                        , HSA.l HSA.Abs 0.0 $ titleY - slopeFactor
                        , HSA.l HSA.Abs titleBarWidth titleY
                        , HSA.z
                        ]
                    , HSA.fill $ Just $ P.hColorOf $ if not beingDragged then _.i900 Palette.yellow else _.i900 Palette.magenta
                    , HSA.stroke $ Just $ P.hColorOf $ if not beingDragged then _.i100 Palette.yellow else _.i100 Palette.magenta
                    , HSA.strokeWidth 1.5
                    ]
                , HS.path
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
                , HS.text
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
                ]
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
        valueFontSize = 9.0
        titleFontSize = 11.0
        connectorRadius = 5.0
        maxChannelsCount = max inletsCount outletsCount
        nodeWidth = titleWidth + (channelStep * Int.toNumber maxChannelsCount)
        titleY = channelBarHeight + bodyHeight
        channelNameShift = connectorRadius + 4.0
        valueOfInlet inletR = latestUpdate <#> _.inlets <#> MapX.mapKeys Tuple.snd >>= Map.lookup inletR
        valueOfOutlet outletR = latestUpdate <#> _.outlets <#> MapX.mapKeys Tuple.snd >>= Map.lookup outletR
        -- fullHeight = channelBarHeight + bodyHeight + channelBarHeight
        slopeFactor = 5.0
        renderValue v =
            HS.text
                [ HSA.fill $ Just $ P.hColorOf Palette.paper
                , HSA.dominant_baseline HSA.Hanging
                , HSA.font_size $ HSA.FontSizeLength $ HSA.Px valueFontSize
                ]
                [ HH.text $ writeChannelRepr v ]
        renderViCValue = ViC.resolve
            { accept : renderValue
            , decline : HSX.none
            , empty : HSX.none
            , missingKey : const HSX.none
            }
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
                , HS.g
                    [ HSA.transform [ HSA.Translate (connectorRadius * 2.0) (-channelBarHeight) ] ]
                    [ maybe HSX.none renderViCValue $ valueOfInlet inletDef.name ]
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
                , HS.g
                    [ HSA.transform [ HSA.Translate (connectorRadius * 2.0) channelBarHeight ] ]
                    [ maybe HSX.none renderViCValue $ valueOfOutlet outletDef.name ]
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



handleAction :: forall sterpr chrepr m. Action sterpr chrepr m -> H.HalogenM (State sterpr chrepr m) (Action sterpr chrepr m) () Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    Receive input ->
        H.modify_ _
            { node = input.node
            , position = input.position
            }
    HeaderClick ->
        H.raise HeaderWasClicked


handleQuery :: forall action output sterpr chrepr m a. Query sterpr chrepr a -> H.HalogenM (State sterpr chrepr m) action () output m (Maybe a)
handleQuery = case _ of
    QueryUpdate changes a -> do
        H.modify_ _ { latestUpdate = Just changes }
        pure $ Just a
    QueryDragStart a -> do
        H.modify_ _ { beingDragged = true }
        pure $ Just a
    QueryDragEnd a -> do
        H.modify_ _ { beingDragged = false }
        pure $ Just a