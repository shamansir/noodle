module App.Style.Calculate where

import Prelude

import Data.Int (toNumber)

import Noodle.Node (Node)
import Noodle.Node (dimensions) as Node
import App.Style (NodeFlow(..), Units)

import Data.Tuple.Nested ((/\), type (/\))


type GetPos = Units -> NodeFlow ->  Number /\ Number
type GetPosByIdx = Units -> NodeFlow -> Int -> Number /\ Number
type GetSize = Units -> NodeFlow ->  Number /\ Number
type GetSizeByNode d = Units -> NodeFlow -> Node d -> Number /\ Number


inletPos :: GetPosByIdx
inletPos u Vertical idx =
    u.slotOuterWidth /\ ((u.slotOuterHeight / 2.0) + (u.slotOuterHeight * toNumber idx))
inletPos u Horizontal idx =
    0.0 /\ toNumber idx


outletPos :: GetPosByIdx
outletPos u Vertical idx =
    ( u.slotOuterWidth + u.nodeBodyWidth) /\ ((u.slotOuterHeight / 2.0) + (u.slotOuterHeight * toNumber idx))
outletPos u Horizontal idx =
    0.0 /\ toNumber idx


inletRectPos :: GetPosByIdx
inletRectPos u Vertical idx =
    ((u.slotOuterWidth - u.slotRadius / 2.0) /\ (u.slotOuterHeight * toNumber idx))
inletRectPos u Horizontal idx =
    0.0 /\ toNumber idx


outletRectPos :: GetPosByIdx
outletRectPos u Vertical idx =
    (u.nodeBodyWidth - u.slotOuterWidth) /\ u.slotOuterHeight * toNumber idx
outletRectPos u Horizontal idx =
    0.0 /\ toNumber idx


bodyPos :: GetPos
bodyPos u Vertical = u.slotOuterWidth /\ 0.0
bodyPos u Horizontal = u.slotOuterWidth /\ 0.0


inletTextPos :: GetPosByIdx
inletTextPos u Vertical idx =
    case inletPos u Vertical idx of
        x /\ y -> (x - u.slotOuterWidth) /\ y
inletTextPos u Horizontal idx = 0.0 /\ 0.0


outletTextPos :: GetPosByIdx
outletTextPos u Vertical idx =
    case outletPos u Vertical idx of
        x /\ y -> (x + u.slotRadius + 5.0) /\ y
outletTextPos u Horizontal idx = 0.0 /\ 0.0


shadowPos :: GetPos
shadowPos u dir = case bodyPos u dir of
    x /\ y -> (x + u.bodyShadowShift) /\ (y + u.bodyShadowShift)


namePos :: GetPos
namePos u Vertical =
    u.slotOuterWidth /\ 0.0
namePos u Horizontal = 0.0 /\ 0.0


slotSize :: GetSize
slotSize u Vertical = u.slotOuterWidth /\ u.slotOuterHeight
slotSize u Horizontal = u.slotOuterWidth /\ u.slotOuterHeight


namePlateSize :: GetSize
namePlateSize u Vertical = u.nodeBodyWidth /\ u.namePlateHeight
namePlateSize u Horizontal = u.nodeBodyWidth /\ u.namePlateHeight


nodeBounds :: forall d. GetSizeByNode d
nodeBounds u flow node =
    let
        inletsCount /\ outletsCount = Node.dimensions node
    in
        case flow of
            Vertical ->
                (u.slotOuterWidth * 2.0 + u.nodeBodyWidth)
                /\ (toNumber (max inletsCount outletsCount) * u.slotOuterHeight)
            Horizontal ->
                (u.slotOuterWidth * 2.0 + u.nodeBodyWidth)
                /\ (toNumber (max inletsCount outletsCount) * u.slotOuterHeight)


nodeBodySize :: forall d. GetSizeByNode d
nodeBodySize u flow node =
    let
        inletsCount /\ outletsCount = Node.dimensions node
    in
        case flow of
            Vertical ->
                u.nodeBodyWidth
                /\ (toNumber (max inletsCount outletsCount) * u.slotOuterHeight)
            Horizontal ->
                (toNumber (max inletsCount outletsCount) * u.slotOuterWidth)
                /\ u.nodeBodyHeight