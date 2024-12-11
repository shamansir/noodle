module Cli.Components.Link where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)

import Type.Data.Symbol (class IsSymbol)

import Control.Monad.State as State
import Control.Monad.Error.Class (class MonadThrow)

import Data.Foldable (for_, foldr)
import Data.Int (floor, toNumber)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Data.Tuple.Nested ((/\), type (/\))

import Blessed ((>~))
import Blessed (line) as B
import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Orientation as Orientation
import Blessed.Internal.BlessedOp (imapState) as Blessed
import Blessed.Internal.NodeKey (RawNodeKey)
import Blessed.Internal.BlessedOp (BlessedOpGet, BlessedOp)
import Blessed.Internal.BlessedSubj (Line)
import Blessed.Internal.Core as Core
import Blessed.Internal.Emitter (class Fires) as E
import Blessed.Internal.JsApi (EventJson)
import Blessed.Internal.NodeKey (next, toRaw) as NodeKey
import Blessed.Internal.NodeKey (type (<^>))
import Blessed.UI.Base.Element.Method (setBack) as Element
import Blessed.UI.Base.Element.Property (left, top, width, height) as Element
import Blessed.UI.Base.Element.PropertySet (setHeight, setLeft, setTop, setWidth) as Element
import Blessed.UI.Base.Node.Method as Node
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Line.Option (ch, fg, orientation, type_) as Line

import Cli.Bounds (Bounds, NodeBounds)
import Cli.Bounds (collect, outletPos, inletPos) as Bounds
import Cli.Keys (PatchBoxKey, NodeBoxKey, LineA, LineB, LineC) as K
import Cli.Keys (lineA, lineB, lineC) as Key
import Cli.Style as Style

import Noodle.Id as Id
import Noodle.Ui.Cli.Palette as Palette


-- TODO: forall state. BlessedOp state Effect


type LinksFrom s =  Map RawNodeKey (Map Id.OutletIndex (LinkState s))
type LinksTo s = Map RawNodeKey (Map Id.InletIndex (LinkState s))


newtype LinkState s =
    LinkState
    { id :: Int
    , inPatch :: Id.Link
    , blessed :: { a :: Core.Blessed s, b :: Core.Blessed s, c :: Core.Blessed s }
    , fromNode :: { key :: K.NodeBoxKey, id :: Id.NodeR }
    , toNode :: { key :: K.NodeBoxKey, id :: Id.NodeR }
    , outletIndex :: Int
    , inletIndex :: Int
    , keys ::
        { a :: K.LineA
        , b :: K.LineB
        , c :: K.LineC
        }
    }


derive instance Newtype (LinkState s) _


type LinkCalc =
    { a :: Bounds
    , b :: Bounds
    , c :: Bounds
    }


type LinkHandler s = forall id. IsSymbol id => LinkState s -> Line <^> id -> EventJson -> BlessedOp s Effect


-- FIXME: pass data from state as arguments and make Links independent from State type


create
    :: forall s ls m
     . MonadThrow Error m
    => Id.Link
    -> { key :: K.NodeBoxKey, id :: Id.NodeR }
    -> Id.OutletIndex
    -> { key :: K.NodeBoxKey, id :: Id.NodeR }
    -> Id.InletIndex
    -> Maybe (LinkState ls)
    -> BlessedOpGet s m (LinkState ls)
create inPatch fromNode (Id.OutletIndex outletIdx) toNode (Id.InletIndex inletIdx) maybePrev = do
    -- maybePrev <- _.lastLink <$> State.get
    from <- Bounds.collect fromNode.id fromNode.key
    to <- Bounds.collect toNode.id toNode.key

    let

        keyLinkA = fromMaybe Key.lineA $ NodeKey.next <$> _.a <$> _.keys <$> unwrap <$> maybePrev
        keyLinkB = fromMaybe Key.lineB $ NodeKey.next <$> _.b <$> _.keys <$> unwrap <$> maybePrev
        keyLinkC = fromMaybe Key.lineC $ NodeKey.next <$> _.c <$> _.keys <$> unwrap <$> maybePrev
        calc = calculate { from, to } (Id.OutletIndex outletIdx) (Id.InletIndex inletIdx)

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
                , inPatch
                , fromNode
                , toNode
                , outletIndex : outletIdx
                , inletIndex : inletIdx
                , blessed : { a : linkA [], b : linkB [], c : linkC [] }
                , keys : { a : keyLinkA, b : keyLinkB, c : keyLinkC }
                }

    pure linkState


calculate :: { from :: NodeBounds, to :: NodeBounds } -> Id.OutletIndex -> Id.InletIndex -> LinkCalc
calculate np (Id.OutletIndex outletIdx) (Id.InletIndex inletIdx) =
    let
        o = Bounds.outletPos np.from outletIdx
        i = Bounds.inletPos np.to inletIdx
        my = floor $ abs (toNumber i.y - toNumber o.y) / 2.0
        acalc =
            if o.y <= i.y then -- outlet above inlet
                { left : o.x, top : o.y, width : 1, height : my }
            else
                { left : i.x, top : i.y, width : 1, height : my }
        bcalc =
            if o.y <= i.y then -- outlet above inlet
                if o.x <= i.x then -- outlet on the left from inlet
                    { left : o.x, top : o.y + my, width : i.x - o.x, height : 1 }
                else
                    { left : i.x, top : o.y + my, width : o.x - i.x, height : 1 }
            else
                if i.x <= o.x then -- inlet on the left from outlet
                    { left : i.x, top : i.y + my, width : o.x - i.x, height : 1 }
                else
                    { left : o.x, top : i.y + my, width : i.x - o.x, height : 1 }
        ccalc =
            if o.y <= i.y then -- outlet above inlet
                { left : i.x, top : o.y + my, width : 1, height : my }
            else
                { left : o.x, top : i.y + my, width : 1, height : my }
    in
    { a : acalc
    , b : bcalc
    , c : ccalc
    }


append :: forall s m. LinkState s -> K.PatchBoxKey -> BlessedOp s m
append (LinkState link) pnk = do
    link.keys.a >~ Element.setBack
    link.keys.b >~ Element.setBack
    link.keys.c >~ Element.setBack
    pnk >~ Node.append link.blessed.a
    pnk >~ Node.append link.blessed.b
    pnk >~ Node.append link.blessed.c


remove :: forall s m. LinkState s -> K.PatchBoxKey -> BlessedOp s m
remove (LinkState link) pnk = do
    pnk >~ Node.remove link.blessed.a
    pnk >~ Node.remove link.blessed.b
    pnk >~ Node.remove link.blessed.c


on :: forall s e m. E.Fires Line e => e -> LinkHandler s -> LinkState s -> BlessedOp s m
on evt handler (LinkState link) = do
    link.keys.a >~ Core.on' evt (handler $ LinkState link)
    link.keys.b >~ Core.on' evt (handler $ LinkState link)
    link.keys.c >~ Core.on' evt (handler $ LinkState link)


update :: forall s m. MonadThrow Error m => LinkState s -> BlessedOp s m
update (LinkState link) = do
    from <- Bounds.collect link.fromNode.id link.fromNode.key
    to <- Bounds.collect link.toNode.id link.toNode.key

    let calc =
            calculate
            { from, to }
            (Id.OutletIndex link.outletIndex)
            (Id.InletIndex link.inletIndex)

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


forget :: forall s. LinkState s -> (LinksFrom s /\ LinksTo s) -> (LinksFrom s /\ LinksTo s)
forget (LinkState props) (linksFrom /\ linksTo) =
    Map.update (Map.delete (Id.OutletIndex props.outletIndex) >>> Just) (NodeKey.toRaw props.fromNode.key) linksFrom
    /\
    Map.update (Map.delete (Id.InletIndex props.inletIndex) >>> Just) (NodeKey.toRaw props.toNode.key) linksTo


store :: forall s. LinkState s -> (LinksFrom s /\ LinksTo s) -> (LinksFrom s /\ LinksTo s)
store link@(LinkState props) (linksFrom /\ linksTo) =
    Map.alter (push (Id.OutletIndex props.outletIndex) link) (NodeKey.toRaw props.fromNode.key) linksFrom
    /\
    Map.alter (push (Id.InletIndex props.inletIndex) link) (NodeKey.toRaw props.toNode.key) linksTo
    {-
    state
        { linksFrom =
            Map.alter (push (OutletIndex props.outletIndex) link) (NodeKey.rawify props.fromNode.key) state.linksFrom
        , linksTo =
            Map.alter (push (InletIndex props.inletIndex) link) (NodeKey.rawify props.toNode.key) state.linksTo
        , lastLink = Just link
        }
    -}


push :: forall s key. Ord key => key -> LinkState s -> Maybe (Map key (LinkState s)) -> Maybe (Map key (LinkState s))
push id link (Just map) = Just $ Map.insert id link map
push id link Nothing = Just $ Map.singleton id link


forgetAllFromTo :: forall s. K.NodeBoxKey -> (LinksFrom s /\ LinksTo s) -> (LinksFrom s /\ LinksTo s)
forgetAllFromTo nbKey (linksFrom /\ linksTo) =
    let
        rawNk = NodeKey.toRaw nbKey
        allLinks :: List (LinkState s)
        allLinks
            =  (Map.values $ fromMaybe Map.empty $ Map.lookup rawNk linksFrom)
            <> (Map.values $ fromMaybe Map.empty $ Map.lookup rawNk linksTo)
    in foldr forget (linksFrom /\ linksTo) allLinks


removeAllOf :: forall s m. K.NodeBoxKey -> K.PatchBoxKey -> LinksFrom s -> LinksTo s -> BlessedOp s m
removeAllOf nbKey pnk linksFrom linksTo = do
    let rawNk = NodeKey.toRaw nbKey
    for_ (fromMaybe Map.empty $ Map.lookup rawNk linksFrom) $ flip remove pnk
    for_ (fromMaybe Map.empty $ Map.lookup rawNk linksTo) $ flip remove pnk


{-
toUnit :: forall s. LinkState s -> LinkState Unit
toUnit (LinkState s) = LinkState $ s
    { blessed = { a : Blessed.imapState ?wh ?wh ?wh, b : ?wh, c : ?wh  }
    }
-}
