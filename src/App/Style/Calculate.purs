module App.Style.Calculate where

import Prelude

import Data.Int (toNumber)
import Data.Vec2 (Vec2, (<+>), (</>), Pos, Size)
import Data.Vec2 as V2
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.Foldable
-- import Data.Set.Ordered (map) as OSet
import Data.Maybe (Maybe(..), fromMaybe)

import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Channel.Shape as Ch

import App.Style
import App.Style.Order as Order


type Index = Int


type GetPos d = Flags -> Style -> NodeFlow -> Node d -> Pos
type GetPosByIdx d = Flags -> Style -> NodeFlow -> Node d -> Int -> Pos
type GetSize d = Flags -> Style -> NodeFlow -> Node d -> Size
type GetSizeF d a = Flags -> Style -> NodeFlow -> Node d -> Order.SizeF a


slotPadding :: Pos
slotPadding = 2.0 <+> 7.0


removeButtonRadius :: Number
removeButtonRadius = 7.0


hasInlets :: NodePart -> Boolean
hasInlets OnlyInlets = true
hasInlets InletsAndOutlets = true
hasInlets UserBodyBetweenSlots = true
hasInlets (UserBodyBetweenSlotsMin _) = true
hasInlets _ = false


hasOutlets :: NodePart -> Boolean
hasOutlets OnlyOutlets = true
hasOutlets InletsAndOutlets = true
hasOutlets UserBodyBetweenSlots = true
hasOutlets (UserBodyBetweenSlotsMin _) = true
hasOutlets _ = false


connectorSize :: Connector -> Size
connectorSize (Square n) = n <+> n
connectorSize (Rect size) = size
connectorSize (Circle radius) = radius * 2.0 <+> radius * 2.0
connectorSize (DoubleCircle _ outerRadius) = outerRadius * 2.0 <+> outerRadius * 2.0


slotArea :: forall d. GetSize d
slotArea _ s Vertical _ =
    V2.w (connectorSize s.slot.connector + s.slot.offset) + (V2.w slotPadding * 2.0) + s.slot.label.maxWidth
    <+>
    V2.h (connectorSize s.slot.connector + s.slot.offset) + (V2.h slotPadding * 2.0)
slotArea _ s Horizontal _ =
    V2.w (connectorSize s.slot.connector + s.slot.offset) + (V2.w slotPadding * 2.0)
    <+>
    V2.h (connectorSize s.slot.connector + s.slot.offset) + (V2.w slotPadding * 2.0) + s.slot.label.maxWidth


inletConnectorPos :: forall d. GetPosByIdx d
inletConnectorPos f s Vertical node idx =
    s.body.margin +
    s.slot.offset +
    (
        connectorOffsetX s.slot.direction
        <+>
        (offsetY
            + (outerHeight * toNumber idx)
            + (outerHeight / 2.0)
        )
    )
    where
        sizeF_ = nodeAreaF f s Vertical node
        offsetY = fromMaybe 0.0 $ Order.sizeBefore sizeF_ hasInlets s.order
        outerHeight = V2.h $ slotArea f s Vertical node
        connectorOffsetX Inside = 0.0 -- V2.w $ slotArea slot flow
        connectorOffsetX Between = 0.0 -- V2.w $ (slotArea slot flow </> V2.vv 2.0) - V2.vv V2.w (connectorSize slot.connector)
        connectorOffsetX Outside = 0.0
inletConnectorPos f s Horizontal node idx =
    s.body.margin +
    s.slot.offset +
    (
        (titleWidth
            + (outerWidth * toNumber idx)
            + (outerWidth / 2.0)
        )
        <+>
        connectorOffsetY s.slot.direction
    )
    where
        outerWidth = V2.w $ slotArea f s Horizontal node
        connectorOffsetY Inside = V2.h $ slotArea f s Horizontal node
        connectorOffsetY Between =
            V2.h (slotArea f s Horizontal node </> V2.vv 2.0) - V2.w (connectorSize s.slot.connector)
        connectorOffsetY Outside = 0.0
        titleWidth =
            if f.hasTitle then V2.w $ titleSize f s Horizontal node else 0.0


inletRectPos :: forall d. GetPosByIdx d
inletRectPos f s Vertical node idx =
    s.slot.offset +
    (
        offsetX s.slot.direction
        <+>
        (offsetY
            + (outerHeight * toNumber idx)
            + (outerHeight / 2.0)
            - (V2.h (connectorSize s.slot.connector) / 2.0)
        )
    )
    where
        sizeF_ = nodeAreaF f s Vertical node
        offsetY = fromMaybe 0.0 $ Order.sizeBefore sizeF_ hasInlets s.order
        outerHeight = V2.h $ slotArea f s Vertical node
        offsetX Inside = V2.w s.body.margin - V2.w (connectorSize s.slot.connector)
        offsetX Between = V2.w (slotArea f s Vertical node) / 2.0
        offsetX Outside = 0.0
inletRectPos f s Horizontal node idx =
    s.body.margin +
    s.slot.offset +
    (
        (offsetX + outerWidth * toNumber idx)
        <+>
        0.0
    )
    where
        outerWidth = V2.w $ slotArea f s Horizontal node
        sizeF_ = nodeAreaF f s Vertical node
        offsetX = fromMaybe 0.0 $ Order.sizeBefore sizeF_ hasInlets s.order


inletTextPos :: forall d. GetPosByIdx d
inletTextPos f s Vertical idx node =
    inletConnectorPos f s Vertical idx node + offsetX s.slot.direction
    where
        offsetX Inside = V2.x' (V2.w (connectorSize s.slot.connector) + V2.w slotPadding)
        offsetX Outside = - V2.x' (V2.w (connectorSize s.slot.connector) + V2.w slotPadding)
        offsetX Between = V2.x' (V2.w (connectorSize s.slot.connector) + V2.w slotPadding)
inletTextPos f s  Horizontal idx node = 0.0 <+> 0.0


outletConnectorPos :: forall d. GetPosByIdx d
outletConnectorPos f s Vertical node idx =
    s.body.margin -
    s.slot.offset +
    (
        (connectorOffsetX s.slot.direction + bodyWidth)
        <+>
        (offsetY + (outerHeight / 2.0) + (outerHeight * toNumber idx))
    )
    where
        bodyWidth = s.body.size
        outerHeight = V2.h $ slotArea f s Vertical node
        connectorOffsetX Inside = 0.0 -- V2.w $ slotArea slot flow
        connectorOffsetX Between = 0.0 -- V2.w $ (slotArea slot flow </> V2.vv 2.0) - V2.vv V2.w (connectorSize slot.connector)
        connectorOffsetX Outside = 0.0
        sizeF_ = nodeAreaF f s Vertical node
        offsetY = fromMaybe 0.0 $ Order.sizeBefore sizeF_ hasOutlets s.order
outletConnectorPos f s Horizontal node idx =
    0.0 <+> toNumber idx


outletTextPos :: forall d. GetPosByIdx d
outletTextPos f s Vertical node idx =
    outletConnectorPos f s Vertical node idx + offsetX s.slot.direction
    where
        offsetX Inside = - V2.x' (V2.w (connectorSize s.slot.connector) + V2.w slotPadding)
        offsetX Outside = V2.x' (V2.w (connectorSize s.slot.connector) + V2.w slotPadding)
        offsetX Between = - V2.x' (V2.w (connectorSize s.slot.connector) * 2.0 + V2.w slotPadding * 2.0)
outletTextPos f s Horizontal node idx = 0.0 <+> 0.0


outletRectPos :: forall d. GetPosByIdx d
outletRectPos f s Vertical node idx =
    s.body.margin -
    s.slot.offset +
    (
        (bodyWidth
            + offsetX s.slot.direction
            + (V2.w (connectorSize s.slot.connector))
        )
        <+>
        (offsetY + (outerHeight * toNumber idx))
    )
    where
        bodyWidth = s.body.size
        outerHeight = V2.h $ slotArea f s Vertical node
        offsetX Inside = -V2.w $ slotArea f s Vertical node
        offsetX Between = 0.0 -- V2.w $ (slotArea slot flow </> V2.vv 2.0) - V2.vv V2.w (connectorSize slot.connector)
        offsetX Outside = -V2.w (connectorSize s.slot.connector) - V2.w slotPadding
        sizeF_ = nodeAreaF f s Vertical node
        offsetY = fromMaybe 0.0 $ Order.sizeBefore sizeF_ hasOutlets s.order
outletRectPos f s Horizontal node idx =
    0.0 <+> toNumber idx


removeButtonPos :: forall d. GetPos d
removeButtonPos f s w n =
    s.body.margin
        + titleSize f s w n
        - V2.vv removeButtonRadius
        - V2.y' 3.0


titlePos :: forall d. GetPos d
titlePos _ s _ _ = s.body.margin


titleTextPos :: forall d. GetPos d
titleTextPos _ s _ _ = s.title.padding


titleSize :: forall d. GetSize d
titleSize _ s Vertical _ =
    bodyWidth <+> titleHeight
    where
        bodyWidth = s.body.size
        titleHeight = s.title.size
titleSize _ s Horizontal _ =
    titleWidth <+> bodyHeight
    where
        bodyHeight = s.body.size
        titleWidth = s.title.size


bodyPos :: forall d. GetPos d
bodyPos f s flow _ =
    if f.hasTitle then
        case s.title.mode of
            InsideBody -> s.body.margin
            OutsideBody -> s.body.margin
                + case flow of
                    Vertical -> V2.h' s.title.size
                    Horizontal -> V2.w' s.title.size
    else 0.0 <+> 0.0


-- excluding the parts outside of the node body
bodySizeF :: forall d. GetSizeF d NodePart
bodySizeF f s flow node =
    let
        inletsCount /\ outletsCount = Node.dimensionsBy (not Ch.isHidden) node
        sizeOf Title =
            if f.hasTitle then
                case s.title.mode of
                    InsideBody -> V2.h $ titleSize f s flow node
                    OutsideBody -> 0.0
            else 0.0
        sizeOf (UserBody n) = if f.controlArea then n else 0.0
        sizeOf OnlyInlets = toNumber inletsCount * V2.h (slotArea f s flow node)
        sizeOf OnlyOutlets = toNumber outletsCount * V2.h (slotArea f s flow node)
        sizeOf InletsAndOutlets = toNumber (max inletsCount outletsCount) * V2.h (slotArea f s flow node)
        sizeOf UserBodyBetweenSlots = sizeOf InletsAndOutlets
        sizeOf (UserBodyBetweenSlotsMin n) =
            if f.controlArea
                then
                    if sizeOf InletsAndOutlets > n
                        then sizeOf InletsAndOutlets
                        else n
                else 0.0
    in sizeOf


-- including the parts outside of the node body
nodeAreaF :: forall d. GetSizeF d NodePart
nodeAreaF f s flow node =
    let
        inletsCount /\ outletsCount = Node.dimensionsBy (not Ch.isHidden) node
        sizeOf Title =
            if f.hasTitle then
                case s.title.mode of
                    InsideBody -> V2.h $ titleSize f s flow node
                    OutsideBody -> V2.h $ titleSize f s flow node
            else 0.0
        sizeOf (UserBody n) = if f.controlArea then n else 0.0
        sizeOf OnlyInlets = toNumber inletsCount * V2.h (slotArea f s flow node)
        sizeOf OnlyOutlets = toNumber outletsCount * V2.h (slotArea f s flow node)
        sizeOf InletsAndOutlets = toNumber (max inletsCount outletsCount) * V2.h (slotArea f s flow node)
        sizeOf UserBodyBetweenSlots = sizeOf InletsAndOutlets
        sizeOf (UserBodyBetweenSlotsMin n) =
            if f.controlArea
                then
                    if sizeOf InletsAndOutlets > n
                        then sizeOf InletsAndOutlets
                        else n
                else 0.0
    in sizeOf


orderBy :: forall d. GetSizeF d NodePart -> GetSize d
orderBy sizeF f s flow node =
    case flow of
        Vertical ->
            bodyWidth <+> bodyHeight
            where
                bodyWidth = s.body.size
                bodyHeight = Order.sizeBy (sizeF f s flow node) s.order
        Horizontal ->
            bodyWidth <+> bodyHeight
            where
                bodyWidth = Order.sizeBy (sizeF f s flow node) s.order
                bodyHeight = s.body.size



-- including the parts outside of the actual body
nodeArea :: forall d. GetSize d
nodeArea f s flow node =
    (V2.w s.body.margin * 2.0 + bodyWidth)
    <+>
    (V2.h s.body.margin * 2.0 + bodyHeight)
    where
        bodyWidth /\ bodyHeight =
            V2.toTuple $ orderBy nodeAreaF f s flow node


bodySize :: forall d. GetSize d
bodySize =
    orderBy bodySizeF


shadowPos :: forall d. GetPos d
shadowPos f s flow node =
    bodyPos f s flow node -- + V2.vv u.bodyShadowShift


bodyInnerOffset :: forall d. GetPos d
bodyInnerOffset f s Vertical node =
    if f.hasTitle then
        case s.title.mode of
            InsideBody -> V2.zh $ titleSize f s Vertical node
            OutsideBody -> zero
     else zero
bodyInnerOffset f s Horizontal node =
    if f.hasTitle then
        case s.title.mode of
            InsideBody -> V2.wz $ titleSize f s Horizontal node
            OutsideBody -> zero
     else zero