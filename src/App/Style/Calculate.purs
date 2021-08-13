module App.Style.Calculate where

import Prelude

import Data.Int (toNumber)
import Data.Vec2 (Vec2, (<+>), (</>), Pos, Size)
import Data.Vec2 as V2
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.Foldable
import Data.Set.Ordered (map) as OSet

import Noodle.Node (Node)
import Noodle.Node as Node

import App.Style (Flags, NodeFlow(..), Units, SlotDirection(..), Order, NodePart(..))


type GetPos = Flags -> Units -> NodeFlow -> Pos
type GetPosByIdx = Flags -> Units -> NodeFlow -> Int -> Pos
type GetSize = Flags -> Units -> NodeFlow -> Size
type GetSizeByNode d = Flags -> Units -> NodeFlow -> Node d -> Size


slotPadding :: Number
slotPadding = 3.5


slotArea :: GetSize
slotArea _ u Vertical = V2.w' (u.slot.radius + slotPadding) + u.slot.area
slotArea _ u Horizontal = V2.h' (u.slot.radius + slotPadding)+ u.slot.area


inletConnectorPos :: SlotDirection -> GetPosByIdx
inletConnectorPos dir t u Vertical idx =
    u.body.margin +
    u.slot.inletsOffset +
    (
        connectorOffsetX dir
        <+>
        (titleHeight
            + (outerHeight * toNumber idx)
            + (outerHeight / 2.0)
        )
    )
    where
        outerHeight = V2.h $ u.slot.area
        connectorOffsetX Inside = 0.0 -- V2.w $ u.slot.area
        connectorOffsetX Between = 0.0 -- V2.w $ (u.slot.area </> V2.vv 2.0) - V2.vv u.slot.radius
        connectorOffsetX Outside = 0.0
        titleHeight =
            if t.hasTitle then V2.h $ titleSize t u Vertical else 0.0
inletConnectorPos dir t u Horizontal idx =
    u.body.margin +
    u.slot.inletsOffset +
    (
        (titleWidth
            + (outerWidth * toNumber idx)
            + (outerWidth / 2.0)
        )
        <+>
        connectorOffsetY dir
    )
    where
        outerWidth = V2.w $ u.slot.area
        connectorOffsetY Inside = V2.h $ u.slot.area
        connectorOffsetY Between =
            V2.h $ (u.slot.area </> V2.vv 2.0) - V2.vv u.slot.radius
        connectorOffsetY Outside = 0.0
        titleWidth =
            if t.hasTitle then V2.w $ titleSize t u Horizontal else 0.0


inletRectPos :: SlotDirection -> GetPosByIdx
inletRectPos dir t u Vertical idx =
    u.slot.inletsOffset +
    (
        offsetX dir
        <+>
        (titleHeight + outerHeight * toNumber idx)
    )
    where
        offsetX Inside = V2.w u.body.margin - u.slot.radius
        offsetX Between = V2.w u.slot.area / 2.0
        offsetX Outside = 0.0
        outerHeight = V2.h $ u.slot.area
        titleHeight = if t.hasTitle then V2.h $ titleSize t u Vertical else 0.0
inletRectPos dir t u Horizontal idx =
    u.body.margin +
    u.slot.inletsOffset +
    (
        (titleWidth + outerWidth * toNumber idx)
        <+>
        0.0
    )
    where
        outerWidth = V2.w $ u.slot.area
        titleWidth = if t.hasTitle then V2.w $ titleSize t u Horizontal else 0.0


inletTextPos :: SlotDirection -> GetPosByIdx
inletTextPos dir t u Vertical idx =
    inletConnectorPos dir t u Vertical idx + offsetX dir
    where
        offsetX Inside = V2.x' (u.slot.radius + slotPadding)
        offsetX Outside = - V2.x' (u.slot.radius + slotPadding)
        offsetX Between = V2.x' (u.slot.radius + slotPadding)
inletTextPos dir t u Horizontal idx = 0.0 <+> 0.0


outletConnectorPos :: SlotDirection -> GetPosByIdx
outletConnectorPos dir t u Vertical idx =
    u.body.margin +
    u.slot.outletsOffset +
    (
        (connectorOffsetX dir + bodyWidth)
        <+>
        (titleHeight + (outerHeight / 2.0) + (outerHeight * toNumber idx))
    )
    where
        bodyWidth = u.body.size
        outerHeight = V2.h u.slot.area
        connectorOffsetX Inside = 0.0 -- V2.w $ u.slot.area
        connectorOffsetX Between = 0.0 -- V2.w $ (u.slot.area </> V2.vv 2.0) - V2.vv u.slot.radius
        connectorOffsetX Outside = 0.0
        titleHeight = if t.hasTitle then V2.h $ titleSize t u Vertical else 0.0
outletConnectorPos dir t u Horizontal idx =
    0.0 <+> toNumber idx


outletTextPos :: SlotDirection -> GetPosByIdx
outletTextPos dir t u Vertical idx =
    outletConnectorPos dir t u Vertical idx + offsetX dir
    where
        offsetX Inside = - V2.x' (u.slot.radius + slotPadding)
        offsetX Outside = V2.x' (u.slot.radius + slotPadding)
        offsetX Between = - V2.x' (u.slot.radius * 2.0 + slotPadding * 2.0)
outletTextPos dir t u Horizontal idx = 0.0 <+> 0.0


outletRectPos :: SlotDirection -> GetPosByIdx
outletRectPos dir t u Vertical idx =
    u.body.margin +
    u.slot.outletsOffset +
    (
        (bodyWidth + offsetX dir)
        <+>
        (titleHeight + (outerHeight / 2.0) + (outerHeight * toNumber idx))
    )
    where
        bodyWidth = u.body.size
        outerHeight = V2.h u.slot.area
        offsetX Inside = -V2.w u.slot.area
        offsetX Between = 0.0 -- V2.w $ (u.slot.area </> V2.vv 2.0) - V2.vv u.slot.radius
        offsetX Outside = -u.slot.radius - slotPadding
        titleHeight = if t.hasTitle then V2.h $ titleSize t u Vertical else 0.0
outletRectPos dir t u Horizontal idx =
    0.0 <+> toNumber idx


titlePos :: GetPos
titlePos _ u _ = u.body.margin


titleTextPos :: GetPos
titleTextPos _ u _ = u.title.padding


titleSize :: GetSize
titleSize t u Vertical =
    bodyWidth <+> titleHeight
    where
        bodyWidth = u.body.size
        titleHeight = u.title.size
titleSize t u Horizontal =
    titleWidth <+> bodyHeight
    where
        bodyHeight = u.body.size
        titleWidth = u.title.size


bodyPos :: GetPos
bodyPos _ u _ = u.body.margin


nodeBounds :: forall d. Order -> GetSizeByNode d
nodeBounds order t u flow node =
    (V2.w u.body.margin * 2.0 + bodyWidth)
    <+>
    (V2.h u.body.margin * 2.0 + bodyHeight)
    where
        bodyWidth /\ bodyHeight = V2.toTuple $ bodySize order t u flow node


bodySize :: forall d. Order -> GetSizeByNode d
bodySize order t u flow node =
    let
        inletsCount /\ outletsCount = Node.dimensions node
        sizeOf Title = if t.hasTitle then V2.h $ titleSize t u Vertical else 0.0
        sizeOf (UserBody n) = n
        sizeOf OnlyInlets = toNumber inletsCount * V2.h u.slot.area
        sizeOf OnlyOutlets = toNumber outletsCount * V2.h u.slot.area
        sizeOf InletsAndOutlets = toNumber (max inletsCount outletsCount) * V2.h u.slot.area
        sizeOf UserBodyBetweenSlots = sizeOf InletsAndOutlets
        sizeOf (UserBodyBetweenSlotsMin n) =
            if sizeOf InletsAndOutlets > n then sizeOf InletsAndOutlets else n
    in
        case flow of
            Vertical ->
                bodyWidth <+> bodyHeight
                where
                    bodyWidth = u.body.size
                    bodyHeight = foldr (+) 0.0 $ OSet.map sizeOf order
            Horizontal ->
                bodyWidth <+> bodyHeight
                where
                    bodyWidth = foldr (+) 0.0 $ OSet.map sizeOf order
                    bodyHeight = u.body.size




shadowPos :: GetPos
shadowPos t u flow =
    bodyPos t u flow -- + V2.vv u.bodyShadowShift


bodyInnerOffset :: SlotDirection -> GetPos
bodyInnerOffset _ t u Vertical =
    if t.hasTitle then V2.zh $ titleSize t u Vertical else zero
bodyInnerOffset _ t u Horizontal =
    if t.hasTitle then V2.wz $ titleSize t u Horizontal else zero