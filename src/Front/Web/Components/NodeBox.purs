module Web.Components.NodeBox where

import Prelude

import Debug as Debug

import Type.Proxy (Proxy)

import Effect.Class (class MonadEffect)

import Data.Array ((:))
import Data.Array (length, snoc) as Array
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber) as Int
import Data.Map (lookup) as Map
import Data.Map.Extra (mapKeys) as MapX
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (unwrap, wrap) as NT
import Data.String (length, toUpper, drop) as String
import Data.Text.Format as T
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Web.Event.Event (preventDefault, stopPropagation) as WE
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (clientX, clientY) as Mouse
import Web.UIEvent.MouseEvent (toEvent) as ME

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.Extra as HSX

import Noodle.Id (FamilyR, InletR, OutletR, family, familyOf, inletRName, outletRName) as Id
import Noodle.Id (Temperament(..))
import Noodle.Toolkit (class MarkToolkit, class HasChRepr)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (NodeChanges, id, shape, family) as RawNode
import Noodle.Repr.ChRepr (class WriteChannelRepr, writeChannelRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (resolve, _reportMissingKey) as ViC
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Ui.Tagging as T
import Noodle.Ui.Tagging.At (ChannelLabel, StatusLine) as At
import Noodle.Ui.Tagging.At (class At) as T

import Front.Shared.Bounds (Position, PositionXY, Size)
import Web.Formatting as WF
import Web.Layer (TargetLayer(..))
import Web.Paths as Paths
import Web.Class.WebRenderer (class WebEditor)
import Web.Components.ValueEditor (EditorId(..)) as ValueEditor
import Web.Components.AppScreen.KeyboardLogic as KL


type Input strepr chrepr m =
    { node :: Raw.Node strepr chrepr m
    , position :: Position
    , size :: Size
    , inMouseFocus :: Boolean
    , isDragging :: Boolean
    , keyboardFocus :: KL.NodeFocus
    }


data MouseFocus chrepr -- TODO: merge with KeyboardFocus
    = NoMouseFocus
    | IsOverInlet RawShape.InletDefR (ValueInChannel chrepr)
    | IsOverOutlet RawShape.OutletDefR (ValueInChannel chrepr)
    | IsOverBody
    | BeingDragged


type State strepr chrepr m =
    { node :: Raw.Node strepr chrepr m
    , position :: Position
    , size :: Size
    , latestUpdate :: Maybe (RawNode.NodeChanges strepr chrepr)
    , mouseFocus :: MouseFocus chrepr
    , keyboardFocus :: KL.NodeFocus
    }


data Action sterpr chrepr m
    = Initialize
    | Receive (Input sterpr chrepr m)
    | MouseMove MouseEvent
    | HeaderClick MouseEvent
    | DragButtonClick MouseEvent
    | RemoveButtonClick MouseEvent
    | InletClick MouseEvent Id.InletR
    | InletValueClick MouseEvent PositionXY Id.InletR (ValueInChannel chrepr)
    | OutletClick MouseEvent Id.OutletR
    | ChangeMouseFocus (MouseFocus chrepr)
    | ClearMouseFocus
    | Skip


data Output strepr chrepr
    = HeaderWasClicked MouseEvent
    | ReportMouseMove MouseEvent
    | InletWasClicked Id.InletR
    | InletValueWasClicked PositionXY Id.InletR ValueEditor.EditorId (ValueInChannel chrepr)
    | OutletWasClicked Id.OutletR PositionXY
    | RemoveButtonWasClicked
    | UpdateStatusBar T.Tag
    | ClearStatusBar
    | RequestDocumentation (Maybe (RawNode.NodeChanges strepr chrepr))


data Query strepr chrepr a
    = ApplyChanges (RawNode.NodeChanges strepr chrepr) a
    | ApplyDragStart a
    | ApplyDragEnd a
    | CallInletValueEditor Id.InletR a


component
    :: forall tk strepr chrepr m
     . MonadEffect m
    => MarkToolkit tk
    => HasChRepr tk chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => T.At At.StatusLine chrepr
    => T.At At.ChannelLabel chrepr
    => WebEditor tk chrepr m
    => Proxy tk
    -> H.Component (Query strepr chrepr) (Input strepr chrepr m) (Output strepr chrepr) m
component ptk =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction ptk
            , handleQuery = handleQuery
            , receive = Just <<< Receive
            }
        }


initialState :: forall sterpr chrepr m. Input sterpr chrepr m -> State sterpr chrepr m
initialState { node, position, size, keyboardFocus, inMouseFocus, isDragging } =
    { node
    , position
    , size
    , latestUpdate : Nothing
    , mouseFocus : if inMouseFocus then IsOverBody else if isDragging then BeingDragged else NoMouseFocus
    , keyboardFocus
    }


-- everything below in this paragraph has to be at top level to allow calculating links' positions from `PatchArea`
channelStep = 55.0 :: Number
titleWidth = 20.0 :: Number
bodyHeight = 72.0 :: Number -- FIXME: could be changed by custom node renderer
channelBarHeight = 15.0 :: Number
connectorRadius = 5.0 :: Number


-- FIXME: find better way to position channels using shared algorithm (`BinPack`?)
inletRelPos :: Int -> PositionXY
inletRelPos idx =
    { x : titleWidth + Int.toNumber idx * channelStep + (connectorRadius / 2.0)
    , y : channelBarHeight / 2.0
    }


-- FIXME: find better way to position channels using shared algorithm (`BinPack`?)
outletRelPos :: Int -> PositionXY
outletRelPos idx =
    { x : titleWidth + Int.toNumber idx * channelStep + (connectorRadius / 2.0)
    , y : channelBarHeight + bodyHeight + (channelBarHeight / 2.0)
    }


render :: forall sterpr chrepr m. T.At At.StatusLine chrepr => T.At At.ChannelLabel chrepr => State sterpr chrepr m -> H.ComponentHTML (Action sterpr chrepr m) () m
render { node, position, latestUpdate, mouseFocus, keyboardFocus } =
    HS.g
        [ HSA.transform [ HSA.Translate position.left position.top ]
        , HE.onMouseMove MouseMove
        -- , HE.onMouseOver $ const $ ChangeFocus IsOverNode
        -- , HE.onMouseOut $ const ClearMouseFocus
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
                    , HE.onMouseOver $ const $ ChangeMouseFocus IsOverBody
                    , HE.onMouseOut $ const ClearMouseFocus
                    -- , HSA.class_ $ ClassName "noodle-capture-events"
                    , HSA.d $ Paths.nodeBodyBg { slope : slopeFactor, width : nodeWidth {- bodyWidth -}, height : bodyHeight }
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
                    [ HH.text $ fitTitle $ Id.family $ Id.familyOf $ RawNode.id node ]
                ]
              : ( if inMouseFocus || inKeyboardFocus then
                    HS.g
                        [ HE.onClick $ if inMouseFocus then DragButtonClick else const Skip
                        , HSA.transform [ HSA.Translate 0.0 0.0 ]
                        ]
                        [ HS.circle
                            [ HSA.r 10.0, HSA.cx 5.0, HSA.cy 5.0
                            , HSA.fill $ Just controlButtonBackColor
                            ]
                        , HS.text
                            [ HSA.fill $ Just controlButtonContentColor
                            , HSA.font_size $ HSA.FontSizeLength $ HSA.Px 22.0
                            , HSA.dominant_baseline HSA.Central
                            , HSA.transform [ HSA.Translate (-1.5) 3.0 ]
                            ]
                            [ HH.text controlButtonContent ]
                        ]
                else HSX.none)
            : ( if inMouseFocus then HS.g
                    [ HE.onClick RemoveButtonClick
                    , HSA.transform [ HSA.Translate nodeWidth (titleY / 2.0)]
                    ]
                    [ HS.circle
                        [ HSA.r 10.0, HSA.cx 5.0, HSA.cy 5.0
                        , HSA.fill $ Just controlButtonBackColor
                        ]
                    , HS.path
                        [ HSA.d $ Paths.removeButton { size : 10.0 } --  $ Paths.removeButton { size : 10.0 }
                        , HSA.fill   $ Just controlButtonContentColor
                        , HSA.stroke $ Just controlButtonContentColor
                        ]
                    ]
                else HSX.none)
            : renderInlets
            : renderOutlets
            : []
        )
    where
        slopeFactor = 5.0
        titleMaxChars = 8 -- bodyHeight / 9.0 (fontCharWidth)
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

        fitTitle title =
            if String.length title <= titleMaxChars then title
            else "…" <> String.drop (String.length title - titleMaxChars) title
        valueOfInlet  inletR =  latestUpdate <#> _.inlets  <#> MapX.mapKeys Tuple.snd >>= Map.lookup inletR  # (ViC._reportMissingKey $ Id.inletRName  inletR)
        valueOfOutlet outletR = latestUpdate <#> _.outlets <#> MapX.mapKeys Tuple.snd >>= Map.lookup outletR # (ViC._reportMissingKey $ Id.outletRName outletR)
        fillForInlet inletDef =
            if isOverInlet inletDef.name then
                Just $ P.hColorOf $ _.i200 $ if inletDef.temp == Hot then Palette.red else Palette.blue
            else Nothing

        strokeForInlet inletDef = Just $ P.hColorOf $ _.i200 $ if inletDef.temp == Hot then Palette.red else Palette.blue
        fillForOulet outletDef = if isOverOutlet outletDef.name then Just $ P.hColorOf $ _.i200 Palette.blue else Nothing
        strokeForOutlet outletDef = Just $ P.hColorOf $ _.i200 $ Palette.blue

        beingDragged = case mouseFocus of
                           BeingDragged -> true
                           _ -> false
        inMouseFocus = case mouseFocus of
                           BeingDragged -> false
                           NoMouseFocus -> false
                           _ -> true
        inKeyboardFocus = case keyboardFocus of
                          KL.NoFocusedNode -> false
                          _ -> true
        isOverInlet inletR = case mouseFocus of
            IsOverInlet inletDef _ -> (_.name $ NT.unwrap inletDef) == inletR
            _ -> false
        isOverOutlet outletR = case mouseFocus of
            IsOverOutlet outletDef _ -> (_.name $ NT.unwrap outletDef) == outletR
            _ -> false

        controlButtonBackColor    = P.hColorOf $ if beingDragged then _.i900 Palette.magenta else
                                    case keyboardFocus of
                                        KL.NoFocusedNode ->    _.i900 Palette.yellow
                                        KL.NodeSelected ->     _.i900 Palette.blue
                                        KL.NodeOpen _ ->       _.i900 Palette.base_
                                        KL.InletsOpen ->       _.i900 Palette.blue
                                        KL.InletSelected _ ->  _.i900 Palette.blue
                                        KL.OutletsOpen ->      _.i900 Palette.blue
                                        KL.OutletSelected _ -> _.i900 Palette.blue
        controlButtonContentColor = P.hColorOf $ if beingDragged then _.i100 Palette.magenta else
                                    case keyboardFocus of
                                        KL.NoFocusedNode ->    _.i100 Palette.yellow
                                        KL.NodeOpen _ ->       _.i300 Palette.blue
                                        KL.NodeSelected ->     _.i100 Palette.blue
                                        KL.InletsOpen ->       _.i300 Palette.green
                                        KL.InletSelected _ ->  _.i100 Palette.green
                                        KL.OutletsOpen ->      _.i300 Palette.purple
                                        KL.OutletSelected _ -> _.i100 Palette.purple
        controlButtonContent =  case keyboardFocus of
                                        KL.NoFocusedNode ->      "✣"
                                        KL.NodeOpen n ->         KL.indexToChar n
                                        KL.NodeSelected ->       "◉"
                                        KL.InletsOpen ->         "⊥"
                                        KL.InletSelected in_ ->  "⊥" <> KL.indexToChar in_
                                        KL.OutletsOpen ->        "⊤"
                                        KL.OutletSelected on_ -> "⊤" <> KL.indexToChar on_

        renderInlet idx inletDef =
            HS.g
                [ HSA.transform [ HSA.Translate inletPos.x inletPos.y ]
                , HE.onClick $ flip InletClick inletDef.name
                , HE.onMouseOver $ const $ ChangeMouseFocus $ IsOverInlet (NT.wrap inletDef) $ valueOfInlet inletDef.name
                , HE.onMouseOut  $ const $ ClearMouseFocus
                , HSA.class_ $ H.ClassName "noodle-capture-events"
                ]
                [ HS.circle
                    [ HSA.fill $ fillForInlet inletDef
                    , HSA.stroke $ strokeForInlet inletDef
                    , HSA.strokeWidth 1.0
                    , HSA.r connectorRadius
                    , HSA.cy $ connectorRadius / 2.0 + 2.0 -- channelBarHeight / 2.0
                    ]
                , case keyboardFocus of
                    KL.InletsOpen ->
                        HS.text
                            [ HSA.x $ -3.0
                            , HSA.fill $ Just $ P.hColorOf $ _.i50 Palette.blue
                            , HSA.transform [ HSA.Translate 0.0 0.5 ]
                            , HSA.dominant_baseline HSA.Hanging
                            , HSA.font_size $ HSA.FontSizeLength $ HSA.Px channelFontSize
                            ]
                            [ HH.text $ show idx ]
                    KL.InletSelected in_ ->
                        HS.text
                            [ HSA.x $ -3.0
                            , HSA.fill $ Just $ P.hColorOf $ if idx == in_ then _.i50 Palette.red else _.i600 Palette.blue
                            , HSA.transform [ HSA.Translate 0.0 0.5 ]
                            , HSA.dominant_baseline HSA.Hanging
                            , HSA.font_size $ HSA.FontSizeLength $ HSA.Px channelFontSize
                            ]
                            [ HH.text $ if idx == in_ then "◉" else show idx ]
                    _ -> HH.text ""
                , HS.text
                    [ HSA.x channelNameShift
                    , HSA.fill $ Just $ P.hColorOf $ _.i50 Palette.blue
                    , HSA.transform [ HSA.Translate 0.0 0.5 ]
                    , HSA.dominant_baseline HSA.Hanging
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px channelFontSize
                    ]
                    [ HH.text $ String.toUpper $ Id.inletRName inletDef.name ]
                , HS.g
                    [ HSA.transform [ HSA.Translate (connectorRadius * 2.0 - 2.0) (-channelBarHeight) ]
                    , HSA.fill $ Just $ P.hColorOf Palette.paper
                    , HSA.dominant_baseline HSA.Hanging
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px valueFontSize
                    , HSA.class_ $ H.ClassName "noodle-capture-events"
                    , HE.onClick $ \mevt -> InletValueClick mevt inletPos inletDef.name $ valueOfInlet inletDef.name
                    ]
                    [ WF.renderFormatting SVG $ T.inlet idx inletDef.name $ valueOfInlet inletDef.name ]
                , HS.rect
                    [ HSA.fill $ Just $ P.hColorOf P.transparent
                    , HSA.width channelStep
                    , HSA.height channelBarHeight
                    , HSA.x $ -connectorRadius - 2.0
                    , HSA.y $ -3.0
                    ]
                ]
            where inletPos = { x : (Int.toNumber idx * channelStep), y : 0.0 }
        renderOulet idx outletDef =
            HS.g
                [ HSA.transform [ HSA.Translate outletPos.x outletPos.y ]
                , HE.onClick $ flip OutletClick outletDef.name
                , HE.onMouseOver $ const $ ChangeMouseFocus $ IsOverOutlet (NT.wrap outletDef) $ valueOfOutlet outletDef.name
                , HE.onMouseOut  $ const $ ClearMouseFocus
                , HSA.class_ $ H.ClassName "noodle-capture-events"
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
                , case keyboardFocus of
                    KL.OutletsOpen ->
                        HS.text
                            [ HSA.x $ -3.0
                            , HSA.fill $ Just $ P.hColorOf $ _.i50 Palette.blue
                            , HSA.transform [ HSA.Translate 0.0 1.0 ]
                            , HSA.dominant_baseline HSA.Hanging
                            , HSA.font_size $ HSA.FontSizeLength $ HSA.Px channelFontSize
                            ]
                            [ HH.text $ show idx ]
                    KL.OutletSelected on_ ->
                        HS.text
                            [ HSA.x $ -3.0
                            , HSA.fill $ Just $ P.hColorOf $ if idx == on_ then _.i50 Palette.red else _.i600 Palette.blue
                            , HSA.transform [ HSA.Translate 0.0 1.0 ]
                            , HSA.dominant_baseline HSA.Hanging
                            , HSA.font_size $ HSA.FontSizeLength $ HSA.Px channelFontSize
                            ]
                            [ HH.text $ if idx == on_ then "◉" else show idx ]
                    _ -> HH.text ""
                , HS.g
                    [ HSA.transform [ HSA.Translate (connectorRadius * 2.0 - 2.0) channelBarHeight ]
                    , HSA.fill $ Just $ P.hColorOf Palette.paper
                    , HSA.dominant_baseline HSA.Hanging
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px valueFontSize
                    ]
                    [ WF.renderFormatting SVG $ T.outlet idx outletDef.name $ valueOfOutlet outletDef.name ]
                , HS.rect
                    [ HSA.fill $ Just $ P.hColorOf P.transparent
                    , HSA.width channelStep
                    , HSA.height channelBarHeight
                    , HSA.x $ -connectorRadius - 2.0
                    , HSA.y 0.0
                    ]
                ]
            where outletPos = { x : (Int.toNumber idx * channelStep), y : 0.0 }
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


handleAction
    :: forall tk strepr chrepr m
     . MonadEffect m
    => MarkToolkit tk
    => HasChRepr tk chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => T.At At.StatusLine chrepr
    => WebEditor tk chrepr m
    => Proxy tk
    -> Action strepr chrepr m
    -> H.HalogenM (State strepr chrepr m) (Action strepr chrepr m) () (Output strepr chrepr) m Unit
handleAction ptk = case _ of
    Initialize ->
        pure unit
    Skip ->
        pure unit
    Receive input ->
        H.modify_ \s -> s
            { node = input.node
            , position = input.position
            , size = input.size
            , keyboardFocus = input.keyboardFocus
            , mouseFocus = if input.inMouseFocus then IsOverBody else if input.isDragging then BeingDragged else s.mouseFocus
            }
    HeaderClick mevt -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        H.raise $ HeaderWasClicked mevt
    DragButtonClick mevt -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        H.raise $ HeaderWasClicked mevt
    RemoveButtonClick mevt -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        H.raise RemoveButtonWasClicked
    InletClick mevt inletR -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        H.raise $ InletWasClicked inletR
    InletValueClick mevt pos inletR vic -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        H.raise $ InletValueWasClicked pos inletR (ValueEditor.EditorId "number") vic
    OutletClick mevt outletR -> do
        H.liftEffect $ WE.stopPropagation $ ME.toEvent mevt
        let
            posX = Int.toNumber $ Mouse.clientX mevt
            posY = Int.toNumber $ Mouse.clientY mevt
        H.raise $ OutletWasClicked outletR { x : posX, y : posY }
    ChangeMouseFocus focus -> do
        H.modify_ _ { mouseFocus = focus }
        case focus of
            NoMouseFocus -> pure unit
            BeingDragged -> pure unit
            IsOverInlet inletDef vic -> do
                state <- H.get
                let inlet = NT.unwrap inletDef
                H.raise $ UpdateStatusBar $ T.inletStatusLine (RawNode.family state.node) inlet.order inlet.name vic
            IsOverOutlet outletDef vic -> do
                state <- H.get
                let outlet = NT.unwrap outletDef
                H.raise $ UpdateStatusBar $ T.outletStatusLine (RawNode.family state.node) outlet.order outlet.name vic
            IsOverBody -> do
                state <- H.get
                case state.latestUpdate of
                    Just latestUpdate -> H.raise $ UpdateStatusBar $ T.nodeStatusLine ptk (RawNode.id state.node) latestUpdate
                    Nothing -> H.raise $ UpdateStatusBar $ T.familyStatusLine ptk $ RawNode.family state.node
                H.raise $ RequestDocumentation state.latestUpdate
    ClearMouseFocus -> do
        H.modify_ _ { mouseFocus = NoMouseFocus }
        H.raise $ ClearStatusBar
    MouseMove evt -> do
        H.raise $ ReportMouseMove evt


handleQuery :: forall action strepr chrepr m a. Query strepr chrepr a -> H.HalogenM (State strepr chrepr m) action () (Output strepr chrepr) m (Maybe a)
handleQuery = case _ of
    ApplyChanges changes a -> do
        H.modify_ _ { latestUpdate = Just changes }
        pure $ Just a
    ApplyDragStart a -> do
        H.modify_ _ { mouseFocus = BeingDragged }
        pure $ Just a
    ApplyDragEnd a -> do
        H.modify_ _ { mouseFocus = NoMouseFocus }
        pure $ Just a
    CallInletValueEditor inletR a -> do -- TODO: make it a query for Editor info instead (instead of emulating click, make `AppScreen` spawn value editor using given info) (EditorInfo -> a)
        { latestUpdate, node } <- H.get
        let inletIdx = RawNode.shape node # RawShape.indexOfInlet inletR # fromMaybe 0
        let valueOfInlet = latestUpdate <#> _.inlets  <#> MapX.mapKeys Tuple.snd >>= Map.lookup inletR # (ViC._reportMissingKey $ Id.inletRName inletR) -- TODO: reuse/simplify
        let inletPos = { x : (Int.toNumber inletIdx * channelStep), y : 0.0 }
        H.raise $ InletValueWasClicked inletPos inletR (ValueEditor.EditorId "number") valueOfInlet
        pure $ Just a
