module App.Style.Calculate where

import Prelude

import Data.Int (toNumber)
import Data.Vec2 (Vec2, (<+>), (</>), Pos, Size)
import Data.Vec2 as V2
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.Foldable
import Data.Set.Ordered (map) as OSet
import Data.Maybe (Maybe(..))

import Noodle.Node (Node)
import Noodle.Node as Node

import App.Style
import App.Style.Order as Order


type Index = Int


type GetPos = Flags -> Style -> NodeFlow -> Pos
type GetPosByIdx = Flags -> Style -> NodeFlow -> Int -> Pos
type GetSize = Flags -> Style -> NodeFlow -> Size
type GetSizeByNode d = Flags -> Style -> NodeFlow -> Node d -> Size


slotPadding :: Number
slotPadding = 3.5


connectorSize :: Connector -> Size
connectorSize (Square n) = n <+> n
connectorSize (Rect size) = size
connectorSize (Circle radius) = radius * 2.0 <+> radius * 2.0
connectorSize (DoubleCircle _ maxRadius) = maxRadius * 2.0 <+> maxRadius * 2.0


connectorRadius :: Connector -> Maybe Number
connectorRadius (Square _) = Nothing
connectorRadius (Rect size) = Nothing
connectorRadius (Circle radius) = Just radius
connectorRadius (DoubleCircle _ maxRadius) = Just maxRadius


slotArea :: GetSize
slotArea _ s Vertical = V2.w (connectorSize s.slot.connector) + slotPadding + s.slot.label.maxWidth <+> 20.0 -- TODO
slotArea _ s Horizontal = V2.h (connectorSize s.slot.connector) + slotPadding + s.slot.label.maxWidth <+> 20.0 -- TODO


inletConnectorPos :: GetPosByIdx
inletConnectorPos f s Vertical idx =
    s.body.margin +
    --s.slot.inletsOffset +
    (
        connectorOffsetX s.slot.direction
        <+>
        (titleHeight
            + (outerHeight * toNumber idx)
            + (outerHeight / 2.0)
        )
    )
    where
        outerHeight = V2.h $ slotArea f s Vertical
        connectorOffsetX Inside = 0.0 -- V2.w $ slotArea slot flow
        connectorOffsetX Between = 0.0 -- V2.w $ (slotArea slot flow </> V2.vv 2.0) - V2.vv V2.w (connectorSize slot.connector)
        connectorOffsetX Outside = 0.0
        titleHeight =
            if f.hasTitle then V2.h $ titleSize f s Vertical else 0.0
inletConnectorPos f s Horizontal idx =
    s.body.margin +
    --s.slot.inletsOffset +
    (
        (titleWidth
            + (outerWidth * toNumber idx)
            + (outerWidth / 2.0)
        )
        <+>
        connectorOffsetY s.slot.direction
    )
    where
        outerWidth = V2.w $ slotArea f s Horizontal
        connectorOffsetY Inside = V2.h $ slotArea f s Horizontal
        connectorOffsetY Between =
            V2.h (slotArea f s Horizontal </> V2.vv 2.0) - V2.w (connectorSize s.slot.connector)
        connectorOffsetY Outside = 0.0
        titleWidth =
            if f.hasTitle then V2.w $ titleSize f s Horizontal else 0.0


inletRectPos :: GetPosByIdx
inletRectPos f s Vertical idx =
    --s.slot.inletsOffset +
    (
        offsetX s.slot.direction
        <+>
        (titleHeight + outerHeight * toNumber idx)
    )
    where
        offsetX Inside = V2.w s.body.margin - V2.w (connectorSize s.slot.connector)
        offsetX Between = V2.w (slotArea f s Vertical) / 2.0
        offsetX Outside = 0.0
        outerHeight = V2.h $ slotArea f s Vertical
        titleHeight = if f.hasTitle then V2.h $ titleSize f s Vertical else 0.0
inletRectPos f s Horizontal idx =
    s.body.margin +
    --s.slot.inletsOffset +
    (
        (titleWidth + outerWidth * toNumber idx)
        <+>
        0.0
    )
    where
        outerWidth = V2.w $ slotArea f s Horizontal
        titleWidth = if f.hasTitle then V2.w $ titleSize f s Horizontal else 0.0


inletTextPos :: GetPosByIdx
inletTextPos f s Vertical idx =
    inletConnectorPos f s Vertical idx + offsetX s.slot.direction
    where
        offsetX Inside = V2.x' (V2.w (connectorSize s.slot.connector) + slotPadding)
        offsetX Outside = - V2.x' (V2.w (connectorSize s.slot.connector) + slotPadding)
        offsetX Between = V2.x' (V2.w (connectorSize s.slot.connector) + slotPadding)
inletTextPos f s  Horizontal idx = 0.0 <+> 0.0


outletConnectorPos :: GetPosByIdx
outletConnectorPos f s Vertical idx =
    s.body.margin +
    --s.slot.outletsOffset +
    (
        (connectorOffsetX s.slot.direction + bodyWidth)
        <+>
        (titleHeight + (outerHeight / 2.0) + (outerHeight * toNumber idx))
    )
    where
        bodyWidth = s.body.size
        outerHeight = V2.h $ slotArea f s Vertical
        connectorOffsetX Inside = 0.0 -- V2.w $ slotArea slot flow
        connectorOffsetX Between = 0.0 -- V2.w $ (slotArea slot flow </> V2.vv 2.0) - V2.vv V2.w (connectorSize slot.connector)
        connectorOffsetX Outside = 0.0
        titleHeight = if f.hasTitle then V2.h $ titleSize f s Vertical else 0.0
outletConnectorPos f s Horizontal idx =
    0.0 <+> toNumber idx


outletTextPos :: GetPosByIdx
outletTextPos f s Vertical idx =
    outletConnectorPos f s Vertical idx + offsetX s.slot.direction
    where
        offsetX Inside = - V2.x' (V2.w (connectorSize s.slot.connector) + slotPadding)
        offsetX Outside = V2.x' (V2.w (connectorSize s.slot.connector) + slotPadding)
        offsetX Between = - V2.x' (V2.w (connectorSize s.slot.connector) * 2.0 + slotPadding * 2.0)
outletTextPos f s Horizontal idx = 0.0 <+> 0.0


outletRectPos :: GetPosByIdx
outletRectPos f s Vertical idx =
    s.body.margin +
    --s.slot.outletsOffset +
    (
        (bodyWidth + offsetX s.slot.direction)
        <+>
        (titleHeight + (outerHeight / 2.0) + (outerHeight * toNumber idx))
    )
    where
        bodyWidth = s.body.size
        outerHeight = V2.h $ slotArea f s Vertical
        offsetX Inside = -V2.w $ slotArea f s Vertical
        offsetX Between = 0.0 -- V2.w $ (slotArea slot flow </> V2.vv 2.0) - V2.vv V2.w (connectorSize slot.connector)
        offsetX Outside = -V2.w (connectorSize s.slot.connector) - slotPadding
        titleHeight = if f.hasTitle then V2.h $ titleSize f s Vertical else 0.0
outletRectPos f s Horizontal idx =
    0.0 <+> toNumber idx


titlePos :: GetPos
titlePos _ s _ = s.body.margin


titleTextPos :: GetPos
titleTextPos _ s _ = s.title.padding


titleSize :: GetSize
titleSize _ s Vertical =
    bodyWidth <+> titleHeight
    where
        bodyWidth = s.body.size
        titleHeight = s.title.size
titleSize _ s Horizontal =
    titleWidth <+> bodyHeight
    where
        bodyHeight = s.body.size
        titleWidth = s.title.size


bodyPos :: GetPos
bodyPos _ u _ = u.body.margin


nodeBounds :: forall d. GetSizeByNode d
nodeBounds f s flow node =
    (V2.w s.body.margin * 2.0 + bodyWidth)
    <+>
    (V2.h s.body.margin * 2.0 + bodyHeight)
    where
        bodyWidth /\ bodyHeight = V2.toTuple $ bodySize f s flow node


bodySize :: forall d. GetSizeByNode d
bodySize f s flow node =
    let
        inletsCount /\ outletsCount = Node.dimensions node
        sizeOf Title = if f.hasTitle then V2.h $ titleSize f s flow else 0.0
        sizeOf (UserBody n) = n
        sizeOf OnlyInlets = toNumber inletsCount * V2.h (slotArea f s flow)
        sizeOf OnlyOutlets = toNumber outletsCount * V2.h (slotArea f s flow)
        sizeOf InletsAndOutlets = toNumber (max inletsCount outletsCount) * V2.h (slotArea f s flow)
        sizeOf UserBodyBetweenSlots = sizeOf InletsAndOutlets
        sizeOf (UserBodyBetweenSlotsMin n) =
            if sizeOf InletsAndOutlets > n then sizeOf InletsAndOutlets else n
    in
        case flow of
            Vertical ->
                bodyWidth <+> bodyHeight
                where
                    bodyWidth = s.body.size
                    bodyHeight = Order.sizeBy sizeOf s.order
            Horizontal ->
                bodyWidth <+> bodyHeight
                where
                    bodyWidth = Order.sizeBy sizeOf s.order
                    bodyHeight = s.body.size



shadowPos :: GetPos
shadowPos f s flow =
    bodyPos f s flow -- + V2.vv u.bodyShadowShift


bodyInnerOffset :: GetPos
bodyInnerOffset f s Vertical =
    if f.hasTitle then V2.zh $ titleSize f s Vertical else zero
bodyInnerOffset f s Horizontal =
    if f.hasTitle then V2.wz $ titleSize f s Horizontal else zero