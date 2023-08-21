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
import Data.List (List)
import Data.Foldable (for_, foldr)

import Control.Monad.State as State

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
import Blessed.UI.Base.Element.Property (left, top, width, height) as Element
import Blessed.UI.Base.Element.PropertySet (setHeight, setLeft, setTop, setWidth) as Element
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Line.Option (ch, fg, orientation, type_) as Line


import Cli.State (State, LinkState(..), OutputIndex(..), InputIndex(..), FromToBounds, LinkCalc)
import Cli.Keys (PatchBoxKey, NodeBoxKey)
import Cli.Keys (lineA, lineB, lineC) as Key
import Cli.Palette as Palette
import Cli.Style as Style
import Cli.Bounds (collect, outputPos, inputPos) as Bounds

-- TODO: forall state. BlessedOp state Effect


type LinkHandler = forall id. IsSymbol id => LinkState -> Line <^> id → EventJson → BlessedOp State Effect


create :: Maybe LinkState -> NodeBoxKey -> OutputIndex -> NodeBoxKey -> InputIndex -> BlessedOpGet State Effect LinkState
create maybePrev fromNode (OutputIndex outputIdx) toNode (InputIndex inputIdx) = do
    from <- Bounds.collect fromNode
    to <- Bounds.collect toNode

    let

        keyLinkA = fromMaybe Key.lineA $ NodeKey.next <$> _.a <$> _.keys <$> unwrap <$> maybePrev
        keyLinkB = fromMaybe Key.lineB $ NodeKey.next <$> _.b <$> _.keys <$> unwrap <$> maybePrev
        keyLinkC = fromMaybe Key.lineC $ NodeKey.next <$> _.c <$> _.keys <$> unwrap <$> maybePrev
        calc = calculate { from, to } (OutputIndex outputIdx) (InputIndex inputIdx)

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

        linkState =
            LinkState
                { id : maybe 0 ((+) 1) $ _.id <$> unwrap <$> maybePrev
                , fromNode
                , toNode
                , outputIndex : outputIdx
                , inputIndex : inputIdx
                , blessed : { a : linkA [], b : linkB [], c : linkC [] }
                , keys : { a : keyLinkA, b : keyLinkB, c : keyLinkC }
                }

    pure linkState


calculate :: FromToBounds -> OutputIndex -> InputIndex -> LinkCalc
calculate np (OutputIndex outputIdx) (InputIndex inputIdx) =
    let
        o = Bounds.outputPos np.from outputIdx
        i = Bounds.inputPos np.to inputIdx
        my = floor $ abs (toNumber i.y - toNumber o.y) / 2.0
        acalc =
            if o.y <= i.y then -- output above input
                { left : o.x, top : o.y, width : 1, height : my }
            else
                { left : i.x, top : i.y, width : 1, height : my }
        bcalc =
            if o.y <= i.y then -- output above input
                if o.x <= i.x then -- output on the left from input
                    { left : o.x, top : o.y + my, width : i.x - o.x, height : 1 }
                else
                    { left : i.x, top : o.y + my, width : o.x - i.x, height : 1 }
            else
                if i.x <= o.x then -- input on the left from output
                    { left : i.x, top : i.y + my, width : o.x - i.x, height : 1 }
                else
                    { left : o.x, top : i.y + my, width : i.x - o.x, height : 1 }
        ccalc =
            if o.y <= i.y then -- output above input
                { left : i.x, top : o.y + my, width : 1, height : my }
            else
                { left : o.x, top : i.y + my, width : 1, height : my }
    in
    { a : acalc
    , b : bcalc
    , c : ccalc
    }


append :: LinkState -> PatchBoxKey -> BlessedOp State Effect
append (LinkState link) pnk = do
    pnk >~ Node.append link.blessed.a
    pnk >~ Node.append link.blessed.b
    pnk >~ Node.append link.blessed.c


remove :: LinkState -> PatchBoxKey -> BlessedOp State Effect
remove (LinkState link) pnk = do
    pnk >~ Node.remove link.blessed.a
    pnk >~ Node.remove link.blessed.b
    pnk >~ Node.remove link.blessed.c


on :: forall e. E.Fires Line e => e -> LinkHandler -> LinkState -> BlessedOp State Effect
on evt handler (LinkState link) = do
    link.keys.a >~ Core.on' evt (handler $ LinkState link)
    link.keys.b >~ Core.on' evt (handler $ LinkState link)
    link.keys.c >~ Core.on' evt (handler $ LinkState link)


update :: LinkState -> BlessedOp State Effect
update (LinkState link) = do
    from <- Bounds.collect link.fromNode
    to <- Bounds.collect link.toNode

    let calc =
            calculate
            { from, to }
            (OutputIndex link.outputIndex)
            (InputIndex link.inputIndex)

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


forget :: LinkState -> State -> State
forget link@(LinkState props) state =
    state
        { linksFrom =
            Map.update (Map.delete props.id >>> Just) (NodeKey.rawify props.fromNode) state.linksFrom
        , linksTo =
            Map.update (Map.delete props.id >>> Just) (NodeKey.rawify props.toNode) state.linksTo
        }


store :: LinkState -> State -> State
store link@(LinkState props) state =
    state
        { linksFrom =
            Map.alter (push props.id link) (NodeKey.rawify props.fromNode) state.linksFrom
        , linksTo =
            Map.alter (push props.id link) (NodeKey.rawify props.toNode) state.linksTo
        , lastLink = Just link
        }


push :: Int -> LinkState -> Maybe (Map Int LinkState) -> Maybe (Map Int LinkState)
push id link (Just map) = Just $ Map.insert id link map
push id link Nothing = Just $ Map.singleton id link


forgetAllFromTo :: NodeBoxKey -> State -> State
forgetAllFromTo nbKey state =
    let
        rawNk = NodeKey.rawify nbKey
        allLinks :: List LinkState
        allLinks = (Map.values $ fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) <> (Map.values $ fromMaybe Map.empty $ Map.lookup rawNk state.linksTo)
    in foldr forget state allLinks


removeAllOf :: NodeBoxKey -> PatchBoxKey -> BlessedOp State Effect
removeAllOf nk pnk = do
    state <- State.get
    let rawNk = NodeKey.rawify nk
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksFrom) $ flip remove pnk
    for_ (fromMaybe Map.empty $ Map.lookup rawNk state.linksTo) $ flip remove pnk
