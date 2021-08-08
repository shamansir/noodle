module App.Style.Calculate where

import Prelude

import Data.Int (toNumber)

import Noodle.Node (Node)
import App.Style (Flags, NodeFlow(..), Units, SlotDirection(..))
import Data.Vec2 (Vec2, (<+>), (</>), Pos, Size)
import Data.Vec2 as V2

import Data.Tuple.Nested ((/\))


type GetPos = Flags -> Units -> NodeFlow -> Pos
type GetPosByIdx = Flags -> Units -> NodeFlow -> Int -> Pos
type GetSize = Flags -> Units -> NodeFlow -> Size
type GetSizeByNode d = Flags -> Units -> NodeFlow -> Node d -> Size


slotArea :: GetSize
slotArea _ u _ = u.slot.area


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
        connectorOffsetX Inside = V2.w $ u.slot.area
        connectorOffsetX Between =
            V2.w $ (u.slot.area </> V2.vv 2.0) - V2.vv u.slot.radius
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


inletRectPos :: GetPosByIdx
inletRectPos t u Vertical idx =
    u.body.margin +
    u.slot.inletsOffset +
    (
        0.0
        <+>
        (titleHeight + outerHeight * toNumber idx)
    )
    where
        outerHeight = V2.h $ u.slot.area
        titleHeight = if t.hasTitle then V2.h $ titleSize t u Vertical else 0.0
inletRectPos t u Horizontal idx =
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
    inletConnectorPos dir t u Vertical idx - V2.x' (u.slot.radius + 5.0)
inletTextPos dir t u Horizontal idx = 0.0 <+> 0.0


outletConnectorPos :: SlotDirection -> GetPosByIdx
outletConnectorPos dir t u Vertical idx =
    0.0 <+> 0.0
    {-
    ( u.slotOuterWidth + u.bodyWidth )
    <+>
    (plateHeight + (u.slotOuterHeight / 2.0) + (u.slotOuterHeight * toNumber idx))
    where plateHeight = V2.h $ titleSize t u Vertical
    -}
outletConnectorPos dir t u Horizontal idx =
    0.0 <+> toNumber idx


outletTextPos :: SlotDirection -> GetPosByIdx
outletTextPos dir t u Vertical idx =
    0.0 <+> 0.0 -- outletRectPos t u Vertical idx + V2.x' (u.slotRadius + 5.0)
outletTextPos dir t u Horizontal idx = 0.0 <+> 0.0


outletRectPos :: GetPosByIdx
outletRectPos t u Vertical idx =
    20.0 <+> 30.0
    {-
    (u.bodyWidth + u.slotOuterWidth)
    <+>
    (plateHeight + u.slotOuterHeight * toNumber idx)
    where plateHeight = V2.h $ titleSize t u Vertical
    -}
outletRectPos t u Horizontal idx =
    0.0 <+> toNumber idx


bodyPos :: GetPos
bodyPos t u Vertical = V2.vv 0.0 -- u.slotOuterWidth <+> 0.0
bodyPos t u Horizontal = V2.vv 0.0 -- u.slotOuterWidth <+> 0.0


shadowPos :: GetPos
shadowPos t u dir =
    bodyPos t u dir -- + V2.vv u.bodyShadowShift


titlePos :: GetPos
titlePos t u Vertical =
    0.0 <+> 0.0 -- u.slotOuterWidth <+> 0.0
titlePos t u Horizontal = 0.0 <+> 0.0


titleTextPos :: GetPos
titleTextPos t u Vertical =
    3.0 <+> 15.0
    -- 3.0 <+> (plateHeight / 2.0)
    -- where plateHeight = V2.h $ titleSize u Vertical
titleTextPos t u Horizontal = 0.0 <+> 0.0


titleSize :: GetSize
titleSize t u Vertical = 200.0 <+> 20.0 -- u.body.width <+> u.namePlateHeight
titleSize t u Horizontal = 200.0 <+> 20.0 -- u.bodyWidth <+> u.namePlateHeight


nodeBounds :: forall d. GetSizeByNode d
nodeBounds t u flow node =
    100.0 <+> 100.0
    {-
    let
        inletsCount /\ outletsCount = Node.dimensions node
    in
        case flow of
            Vertical ->
                (u.slotOuterWidth * 2.0 + u.bodyWidth)
                <+>
                (u.nodePadding + plateHeight + toNumber (max inletsCount outletsCount) * u.slotOuterHeight)
                where plateHeight = V2.h $ titleSize u Vertical
            Horizontal ->
                (u.slotOuterWidth * 2.0 + u.bodyWidth)
                <+>
                (toNumber (max inletsCount outletsCount) * u.slotOuterHeight)
    -}


bodySize :: forall d. GetSizeByNode d
bodySize t u flow node =
    100.0 <+> 100.0
    {- let
        inletsCount /\ outletsCount = Node.dimensions node
    in
        case flow of
            Vertical ->
                u.bodyWidth
                <+>
                (plateHeight + toNumber (max inletsCount outletsCount) * u.slotOuterHeight)
                where plateHeight = V2.h $ titleSize u Vertical
            Horizontal ->
                (toNumber (max inletsCount outletsCount) * u.slotOuterWidth)
                <+>
                u.bodyHeight -}