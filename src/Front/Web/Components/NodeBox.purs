module Web.Components.NodeBox where

import Prelude

import Effect.Class (class MonadEffect)

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
import Data.Newtype (unwrap, wrap) as NT
import Data.Text.Format as T

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.Extra as HSX

import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent) as ME
import Web.UIEvent.MouseEvent (clientX, clientY) as Mouse
import Web.Event.Event (preventDefault, stopPropagation) as WE
import Web.HTML.Common (ClassName(..))

import Noodle.Id (FamilyR, InletR, OutletR, family, familyOf, inletRName, outletRName) as Id
import Noodle.Id (Temperament(..))
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (NodeChanges, id, shape, family) as RawNode
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Repr.ChRepr (class WriteChannelRepr, writeChannelRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (resolve, _reportMissingKey) as ViC

import Noodle.Ui.Tagging.At (ChannelLabel, StatusLine) as At
import Noodle.Ui.Tagging.At (class At) as T

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Ui.Tagging as T

import Web.Paths as Paths
import Web.Formatting as WF


type Input strepr chrepr m =
    { node :: Raw.Node strepr chrepr m
    , position :: { left :: Number, top :: Number }
    }


data MFocus chrepr
    = IsOverInlet RawShape.InletDefR (ValueInChannel chrepr)
    | IsOverOutlet RawShape.OutletDefR (ValueInChannel chrepr)
    | IsOverBody


type State strepr chrepr m =
    { node :: Raw.Node strepr chrepr m
    , position :: { left :: Number, top :: Number }
    , beingDragged :: Boolean
    , latestUpdate :: Maybe (RawNode.NodeChanges strepr chrepr)
    , mouseFocus :: Maybe (MFocus chrepr)
    }


data Action sterpr chrepr m
    = Initialize
    | Receive (Input sterpr chrepr m)
    | MouseMove MouseEvent
    | HeaderClick MouseEvent
    | InletClick  MouseEvent Id.InletR
    | OutletClick MouseEvent Id.OutletR
    | ChangeFocus (MFocus chrepr)
    | ClearFocus


data Output
    = HeaderWasClicked
    | ReportMouseMove  MouseEvent
    | InletWasClicked  Id.InletR
    | OutletWasClicked Id.OutletR { x :: Number, y :: Number }
    | UpdateStatusBar T.Tag
    | ClearStatusBar


data Query strepr chrepr a
    = ApplyChanges (RawNode.NodeChanges strepr chrepr) a
    | ApplyDragStart a
    | ApplyDragEnd a


component :: forall strepr chrepr m. MonadEffect m => T.At At.StatusLine chrepr => T.At At.ChannelLabel chrepr => H.Component (Query strepr chrepr) (Input strepr chrepr m) Output m
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
    , mouseFocus : Nothing
    }


-- everything below in this paragraph has to be at top level to allow calculating links' positions from `PatchArea`
channelStep = 55.0 :: Number
titleWidth = 20.0 :: Number
bodyHeight = 70.0 :: Number -- FIXME: could be changed by custom node renderer
channelBarHeight = 15.0 :: Number
connectorRadius = 5.0 :: Number


-- FIXME: find better way to position channels using shared algorithm (`BinPack`?)
inletRelPos :: Int -> { x :: Number, y :: Number }
inletRelPos idx =
    { x : titleWidth + Int.toNumber idx * channelStep + (connectorRadius / 2.0)
    , y : channelBarHeight / 2.0
    }


-- FIXME: find better way to position channels using shared algorithm (`BinPack`?)
outletRelPos :: Int -> { x :: Number, y :: Number }
outletRelPos idx =
    { x : titleWidth + Int.toNumber idx * channelStep + (connectorRadius / 2.0)
    , y : channelBarHeight + bodyHeight + (channelBarHeight / 2.0)
    }


render :: forall sterpr chrepr m. T.At At.StatusLine chrepr => T.At At.ChannelLabel chrepr => State sterpr chrepr m -> H.ComponentHTML (Action sterpr chrepr m) () m
render { node, position, latestUpdate, beingDragged, mouseFocus } =
    HS.g
        [ HSA.transform [ HSA.Translate position.left position.top ]
        , HE.onMouseMove MouseMove
        ]
        (
            HS.g
                [ HE.onClick HeaderClick ]
                [ HS.path
                    [ HSA.transform [ HSA.Translate (-2.0) channelBarHeight ]
                    , HSA.d $ Paths.nodeTitle { slope : slopeFactor, width : titleBarWidth, height : titleY - channelBarHeight }
                    , HSA.fill   $ Just $ P.hColorOf $ if not beingDragged then _.i900 Palette.yellow else _.i900 Palette.magenta
                    , HSA.stroke $ Just $ P.hColorOf $ if not beingDragged then _.i100 Palette.yellow else _.i100 Palette.magenta
                    , HSA.strokeWidth 1.5
                    ]
                , HS.path
                    [ HSA.transform [ HSA.Translate titleBarWidth channelBarHeight ]
                    , HE.onMouseOver $ const $ ChangeFocus IsOverBody
                    , HE.onMouseOut $ const ClearFocus
                    -- , HSA.class_ $ ClassName "noodle-capture-events"
                    , HSA.d $ Paths.nodeBodyBg { slope : slopeFactor, width : nodeWidth, height : bodyHeight }
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
        channelBarWidth = nodeWidth - titleWidth
        titleBarWidth = titleWidth - slopeFactor
        channelFontSize = 9.0
        valueFontSize = 9.0
        titleFontSize = 11.0
        maxChannelsCount = max inletsCount outletsCount
        bodyWidth = channelStep * Int.toNumber maxChannelsCount
        nodeWidth = titleWidth + bodyWidth
        titleY = channelBarHeight + bodyHeight
        channelNameShift = connectorRadius + 4.0
        slopeFactor = 5.0
        valueOfInlet  inletR =  latestUpdate <#> _.inlets  <#> MapX.mapKeys Tuple.snd >>= Map.lookup inletR  # (ViC._reportMissingKey $ Id.inletRName  inletR)
        valueOfOutlet outletR = latestUpdate <#> _.outlets <#> MapX.mapKeys Tuple.snd >>= Map.lookup outletR # (ViC._reportMissingKey $ Id.outletRName outletR)
        fillForInlet inletDef =
            if isOverInlet inletDef.name then
                Just $ P.hColorOf $ _.i200 $ if inletDef.temp == Hot then Palette.red else Palette.blue
            else Nothing
        strokeForInlet inletDef = Just $ P.hColorOf $ _.i200 $ if inletDef.temp == Hot then Palette.red else Palette.blue
        fillForOulet outletDef = if isOverOutlet outletDef.name then Just $ P.hColorOf $ _.i200 Palette.blue else Nothing
        strokeForOutlet outletDef = Just $ P.hColorOf $ _.i200 $ Palette.blue
        isOverInlet inletR = case mouseFocus of
            Just (IsOverInlet inletDef _) -> (_.name $ NT.unwrap inletDef) == inletR
            _ -> false
        isOverOutlet outletR = case mouseFocus of
            Just (IsOverOutlet outletDef _) -> (_.name $ NT.unwrap outletDef) == outletR
            _ -> false
        renderInlet idx inletDef =
            HS.g
                [ HSA.transform [ HSA.Translate (Int.toNumber idx * channelStep) 0.0 ]
                , HE.onClick $ flip InletClick inletDef.name
                , HE.onMouseOver $ const $ ChangeFocus $ IsOverInlet (NT.wrap inletDef) $ valueOfInlet inletDef.name
                , HE.onMouseOut  $ const $ ClearFocus
                , HSA.class_ $ ClassName "noodle-capture-events"
                ]
                [ HS.circle
                    [ HSA.fill $ fillForInlet inletDef
                    , HSA.stroke $ strokeForInlet inletDef
                    , HSA.strokeWidth 1.0
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
                    [ HSA.transform [ HSA.Translate (connectorRadius * 2.0) (-channelBarHeight) ]
                    , HSA.fill $ Just $ P.hColorOf Palette.paper
                    , HSA.dominant_baseline HSA.Hanging
                    ]
                    [ WF.renderFormatting $ T.inlet idx inletDef.name $ valueOfInlet inletDef.name ]
                ]
        renderOulet idx outletDef =
            HS.g
                [ HSA.transform [ HSA.Translate (Int.toNumber idx * channelStep) 0.0 ]
                , HE.onClick $ flip OutletClick outletDef.name
                , HE.onMouseOver $ const $ ChangeFocus $ IsOverOutlet (NT.wrap outletDef) $ valueOfOutlet outletDef.name
                , HE.onMouseOut  $ const $ ClearFocus
                , HSA.class_ $ ClassName "noodle-capture-events"
                ] -- TODO reverse order so that outlets align to the right side, or even better to bottom right corner
                [ HS.circle
                    [ HSA.fill $ fillForOulet outletDef
                    , HSA.stroke $ strokeForOutlet outletDef
                    , HSA.strokeWidth 1.0
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
                    [ HSA.transform [ HSA.Translate (connectorRadius * 2.0) channelBarHeight ]
                    , HSA.fill $ Just $ P.hColorOf Palette.paper
                    , HSA.dominant_baseline HSA.Hanging
                    ]
                    [ WF.renderFormatting $ T.outlet idx outletDef.name $ valueOfOutlet outletDef.name ]
                ]
        renderInlets =
            HS.g
                [ HSA.transform [ HSA.Translate titleWidth 0.0 ] ]
                (
                    HS.path
                        [ HSA.d $ Paths.channelBarTop { slope : slopeFactor, width : channelBarWidth, height : channelBarHeight }
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
                        [ HSA.d $ Paths.channelBarBottom { slope : slopeFactor, width : channelBarWidth, height : channelBarHeight }
                        , HSA.fill $ Just $ P.hColorOf $ _.i900 Palette.blue
                        ]
                    : [ HS.g
                        [ HSA.transform [ HSA.Translate 3.0 0.0 ] ]
                        $ mapWithIndex renderOulet outletsDefs ]
                )



handleAction :: forall sterpr chrepr m. MonadEffect m => T.At At.StatusLine chrepr => Action sterpr chrepr m -> H.HalogenM (State sterpr chrepr m) (Action sterpr chrepr m) () Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    Receive input ->
        H.modify_ _
            { node = input.node
            , position = input.position
            }
    HeaderClick mevt -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        H.raise HeaderWasClicked
    InletClick mevt inletR -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        H.raise $ InletWasClicked inletR
    OutletClick mevt outletR -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        let
            posX = Int.toNumber $ Mouse.clientX mevt
            posY = Int.toNumber $ Mouse.clientY mevt
        H.raise $ OutletWasClicked outletR { x : posX, y : posY }
    ChangeFocus focus -> do
        H.modify_ _ { mouseFocus = Just focus }
        case focus of
            IsOverInlet inletDef vic -> do
                state <- H.get
                let inlet = NT.unwrap inletDef
                H.raise $ UpdateStatusBar $ T.inletStatusLine (RawNode.family state.node) inlet.order inlet.name vic
            IsOverOutlet outletDef vic -> do
                state <- H.get
                let outlet = NT.unwrap outletDef
                H.raise $ UpdateStatusBar $ T.outletStatusLine (RawNode.family state.node) outlet.order outlet.name vic
            IsOverBody ->
                pure unit
    ClearFocus -> do
        H.modify_ _ { mouseFocus = Nothing }
        H.raise $ ClearStatusBar
    MouseMove evt -> do
        H.raise $ ReportMouseMove evt


handleQuery :: forall action output sterpr chrepr m a. Query sterpr chrepr a -> H.HalogenM (State sterpr chrepr m) action () output m (Maybe a)
handleQuery = case _ of
    ApplyChanges changes a -> do
        H.modify_ _ { latestUpdate = Just changes }
        pure $ Just a
    ApplyDragStart a -> do
        H.modify_ _ { beingDragged = true }
        pure $ Just a
    ApplyDragEnd a -> do
        H.modify_ _ { beingDragged = false }
        pure $ Just a