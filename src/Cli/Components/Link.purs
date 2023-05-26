module Cli.Components.Link where

import Prelude

import Effect (Effect)

import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Type.Data.Symbol (class IsSymbol)
import Data.Ord (abs)
import Data.Map (Map)
import Data.Map as Map

import Blessed ((>~), (~<))
import Blessed (line) as B

import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Orientation as Orientation
import Blessed.Core.Border as Border
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOpGet, BlessedOp)
import Blessed.Internal.BlessedSubj (Line)
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.Internal.NodeKey (next, rawify) as NodeKey
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.Emitter (class Fires) as E

import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Base.Element.Property (left, top) as Element
import Blessed.UI.Base.Element.PropertySet (setHeight, setLeft, setTop, setWidth) as Element
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Line.Option (ch, fg, orientation, type_) as Line


import Cli.State (State, Link(..), OutletIndex(..), InletIndex(..), NodePositions, LinkCalc)
import Cli.Keys (PatchBoxKey, NodeBoxKey)
import Cli.Keys (lineA, lineB, lineC) as Key
import Cli.Palette as Palette
import Cli.Style as Style

-- TODO: forall state. BlessedOp state Effect


type LinkHandler = forall id. IsSymbol id => Link -> Line <^> id → EventJson → BlessedOp State Effect


create :: Maybe Link -> NodeBoxKey -> OutletIndex -> NodeBoxKey -> InletIndex -> BlessedOpGet State Effect Link
create maybePrev fromNode (OutletIndex outletIdx) toNode (InletIndex intletIdx) = do
    fromNodeLeft <- Element.left ~< fromNode
    fromNodeTop <- Element.top ~< fromNode
    toNodeLeft <- Element.left ~< toNode
    toNodeTop <- Element.top ~< toNode
    let

        keyLinkA = fromMaybe Key.lineA $ NodeKey.next <$> _.a <$> _.keys <$> unwrap <$> maybePrev
        keyLinkB = fromMaybe Key.lineB $ NodeKey.next <$> _.b <$> _.keys <$> unwrap <$> maybePrev
        keyLinkC = fromMaybe Key.lineC $ NodeKey.next <$> _.c <$> _.keys <$> unwrap <$> maybePrev
        calc = calculate { fromNodeLeft, fromNodeTop, toNodeLeft, toNodeTop } (OutletIndex outletIdx) (InletIndex intletIdx)

        -- this.link.a = blessed.line({ left : calc.a.left, top : calc.a.top, width : calc.a.width, height : calc.a.height, orientation : 'vertical', type : 'bg', ch : '≀', fg : PALETTE[8] });
        -- this.link.b = blessed.line({ left : calc.b.left, top : calc.b.top, width : calc.b.width, height : calc.b.height, orientation : 'horizontal', type : 'bg', ch : '∼', fg : PALETTE[8] });
        -- this.link.c = blessed.line({ left : calc.c.left, top : calc.c.top, width : calc.c.width, height : calc.c.height, orientation : 'vertical', type : 'bg', ch : '≀', fg : PALETTE[8] });

        linkA = B.line keyLinkA (
                    [ Box.left $ Offset.px calc.a.left
                    , Box.top $ Offset.px calc.a.top
                    , Box.width $ Dimension.px calc.a.width
                    , Box.height $ Dimension.px calc.a.height
                    ] <> Style.linkA )

        linkB = B.line keyLinkB (
                    [ Box.left $ Offset.px calc.b.left
                    , Box.top $ Offset.px calc.b.top
                    , Box.width $ Dimension.px calc.b.width
                    , Box.height $ Dimension.px calc.b.height
                    ] <> Style.linkB )

        linkC = B.line keyLinkC (
                    [ Box.left $ Offset.px calc.c.left
                    , Box.top $ Offset.px calc.c.top
                    , Box.width $ Dimension.px calc.c.width
                    , Box.height $ Dimension.px calc.c.height
                    ] <> Style.linkC )

        link =
            Link
                { id : maybe 0 ((+) 1) $ _.id <$> unwrap <$> maybePrev
                , fromNode
                , toNode
                , outletIndex : outletIdx
                , inletIndex : intletIdx
                , blessed : { a : linkA [], b : linkB [], c : linkC [] }
                , keys : { a : keyLinkA, b : keyLinkB, c : keyLinkC }
                }

    pure link


calculate :: NodePositions -> OutletIndex -> InletIndex -> LinkCalc
calculate np (OutletIndex outletIdx) (InletIndex intletIdx) =
    let
        xo = np.fromNodeLeft + (outletIdx * 6)
        yo = np.fromNodeTop + 3
        xi = np.toNodeLeft + (intletIdx * 6)
        yi = np.toNodeTop + 1
        my = floor $ abs (toNumber yi - toNumber yo) / 2.0
        acalc =
            if yo <= yi then -- outlet above inlet
                { left : xo, top : yo, width : 1, height : my }
            else
                { left : xi, top : yi, width : 1, height : my }
        bcalc =
            if yo <= yi then -- outlet above inlet
                if xo <= xi then -- outlet on the left from inlet
                    { left : xo, top : yo + my, width : xi - xo, height : 1 }
                else
                    { left : xi, top : yo + my, width : xo - xi, height : 1 }
            else
                if xi <= xo then -- inlet on the left from outlet
                    { left : xi, top : yi + my, width : xo - xi, height : 1 }
                else
                    { left : xo, top : yi + my, width : xi - xo, height : 1 }
        ccalc =
            if yo <= yi then -- outlet above inlet
                { left : xi, top : yo + my, width : 1, height : my }
            else
                { left : xo, top : yi + my, width : 1, height : my }
    in
    { a : acalc
    , b : bcalc
    , c : ccalc
    }


append :: Link -> PatchBoxKey -> BlessedOp State Effect
append (Link link) pnk = do
    pnk >~ Node.append link.blessed.a
    pnk >~ Node.append link.blessed.b
    pnk >~ Node.append link.blessed.c


remove :: Link -> PatchBoxKey -> BlessedOp State Effect
remove (Link link) pnk = do
    pnk >~ Node.remove link.blessed.a
    pnk >~ Node.remove link.blessed.b
    pnk >~ Node.remove link.blessed.c


on :: forall e. E.Fires Line e => e -> LinkHandler -> Link -> BlessedOp State Effect
on evt handler (Link link) = do
    link.keys.a >~ Core.on' evt (handler $ Link link)
    link.keys.b >~ Core.on' evt (handler $ Link link)
    link.keys.c >~ Core.on' evt (handler $ Link link)


update :: Link -> BlessedOp State Effect
update (Link link) = do
    fromNodeLeft <- Element.left ~< link.fromNode
    fromNodeTop <- Element.top ~< link.fromNode
    toNodeLeft <- Element.left ~< link.toNode
    toNodeTop <- Element.top ~< link.toNode

    let calc =
            calculate
            { fromNodeLeft, fromNodeTop, toNodeLeft, toNodeTop }
            (OutletIndex link.outletIndex)
            (InletIndex link.inletIndex)

    link.keys.a >~ Element.setLeft $ Offset.px calc.a.left
    link.keys.a >~ Element.setTop $ Offset.px calc.a.top
    link.keys.a >~ Element.setWidth $ Dimension.px calc.a.width
    link.keys.a >~ Element.setHeight $ Dimension.px calc.a.height

    link.keys.b >~ Element.setLeft $ Offset.px calc.b.left
    link.keys.b >~ Element.setTop $ Offset.px calc.b.top
    link.keys.b >~ Element.setWidth $ Dimension.px calc.b.width
    link.keys.b >~ Element.setHeight $ Dimension.px calc.b.height

    link.keys.c >~ Element.setLeft $ Offset.px calc.c.left
    link.keys.c >~ Element.setTop $ Offset.px calc.c.top
    link.keys.c >~ Element.setWidth $ Dimension.px calc.c.width
    link.keys.c >~ Element.setHeight $ Dimension.px calc.c.height


forget :: Link -> State -> State
forget link@(Link props) state =
    state
        { linksFrom =
            Map.update (Map.delete props.id >>> Just) (NodeKey.rawify props.fromNode) state.linksFrom
        , linksTo =
            Map.update (Map.delete props.id >>> Just) (NodeKey.rawify props.toNode) state.linksTo
        }


store :: Link -> State -> State
store link@(Link props) state =
    state
        { linksFrom =
            Map.alter (push props.id link) (NodeKey.rawify props.fromNode) state.linksFrom
        , linksTo =
            Map.alter (push props.id link) (NodeKey.rawify props.toNode) state.linksTo
        , lastLink = Just link
        }



push :: Int -> Link -> Maybe (Map Int Link) -> Maybe (Map Int Link)
push id link (Just map) = Just $ Map.insert id link map
push id link Nothing = Just $ Map.singleton id link