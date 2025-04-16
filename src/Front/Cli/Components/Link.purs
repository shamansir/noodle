module Cli.Components.Link where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

import Type.Data.Symbol (class IsSymbol)

import Control.Monad.State as State
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Foldable (for_, foldr)
import Data.Int (floor, toNumber)
import Data.List (List)
import Data.List (head) as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (abs)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Blessed ((>~))
import Blessed (line) as B
import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Orientation as Orientation
import Blessed.Internal.BlessedOp (lift, runM) as Blessed
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


type LinksCmps s = Map Id.LinkR (LinkCmpState s) -- Map Id.PatchR (Map Id.Link (LinkCmpState Unit))


newtype LinkCmpState s =
    LinkCmpState
    { linkId :: Id.LinkR
    , blessed :: { a :: Core.Blessed s, b :: Core.Blessed s, c :: Core.Blessed s }
    , from :: { node :: K.NodeBoxKey, outletIndex :: Int }
    , to   :: { node :: K.NodeBoxKey, inletIndex  :: Int }
    , keys ::
        { a :: K.LineA
        , b :: K.LineB
        , c :: K.LineC
        }
    }


derive instance Newtype (LinkCmpState s) _


type LinkCalc =
    { a :: Bounds
    , b :: Bounds
    , c :: Bounds
    }


type LinkHandler s = forall id. IsSymbol id => LinkCmpState s -> Line <^> id -> EventJson -> BlessedOp s Effect



create
    :: forall s ls m
     . MonadThrow Error m
    => Id.LinkR
    -> { node :: K.NodeBoxKey, outletIndex :: Int }
    -> { node :: K.NodeBoxKey, inletIndex  :: Int }
    -> Maybe (LinkCmpState ls)
    -> BlessedOpGet s m (LinkCmpState ls)
create linkId linkFrom linkTo maybePrev = do

    -- maybePrev <- _.lastLink <$> State.get
    from <- Bounds.collect (Id.startsFrom linkId) linkFrom.node
    to   <- Bounds.collect (Id.goesTo     linkId) linkTo.node

    let

        keyLinkA = fromMaybe Key.lineA $ NodeKey.next <$> _.a <$> _.keys <$> unwrap <$> maybePrev
        keyLinkB = fromMaybe Key.lineB $ NodeKey.next <$> _.b <$> _.keys <$> unwrap <$> maybePrev
        keyLinkC = fromMaybe Key.lineC $ NodeKey.next <$> _.c <$> _.keys <$> unwrap <$> maybePrev
        calc = calculate { from, to } (Id.OutletIndex linkFrom.outletIndex) (Id.InletIndex linkTo.inletIndex)

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
            LinkCmpState
                { linkId
                , from : linkFrom
                , to   : linkTo
                , blessed : { a : linkA [], b : linkB [], c : linkC [] }
                , keys    : { a : keyLinkA, b : keyLinkB, c : keyLinkC }
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


append :: forall s m. LinkCmpState s -> K.PatchBoxKey -> BlessedOp s m
append (LinkCmpState link) pnk = do
    link.keys.a >~ Element.setBack
    link.keys.b >~ Element.setBack
    link.keys.c >~ Element.setBack
    pnk >~ Node.append link.blessed.a
    pnk >~ Node.append link.blessed.b
    pnk >~ Node.append link.blessed.c


remove :: forall s m. LinkCmpState s -> K.PatchBoxKey -> BlessedOp s m
remove (LinkCmpState link) pnk = do
    pnk >~ Node.remove link.blessed.a
    pnk >~ Node.remove link.blessed.b
    pnk >~ Node.remove link.blessed.c


on :: forall s e m. E.Fires Line e => e -> LinkHandler s -> LinkCmpState s -> BlessedOp s m
on evt handler (LinkCmpState link) = do
    link.keys.a >~ Core.on' evt (handler $ LinkCmpState link)
    link.keys.b >~ Core.on' evt (handler $ LinkCmpState link)
    link.keys.c >~ Core.on' evt (handler $ LinkCmpState link)


update :: forall s m. MonadThrow Error m => LinkCmpState s -> BlessedOp s m
update (LinkCmpState link) = do
    from <- Bounds.collect (Id.startsFrom link.linkId) link.from.node
    to   <- Bounds.collect (Id.goesTo     link.linkId) link.to.node

    let calc =
            calculate
            { from, to }
            (Id.OutletIndex link.from.outletIndex)
            (Id.InletIndex  link.to.inletIndex)

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


-- TODO Move functions below into the `State` module?


of_ :: forall s. LinkCmpState s -> Id.LinkR
of_ = unwrap >>> _.linkId


forget :: forall s. LinkCmpState s -> LinksCmps s -> LinksCmps s
forget (LinkCmpState props) =
    Map.delete props.linkId


store :: forall s. LinkCmpState s -> LinksCmps s -> LinksCmps s
store link@(LinkCmpState props) =
    Map.insert props.linkId link


{-
push :: forall s key. Ord key => key -> LinkCmpState s -> Maybe (Map key (LinkCmpState s)) -> Maybe (Map key (LinkCmpState s))
push id link (Just map) = Just $ Map.insert id link map
push id link Nothing = Just $ Map.singleton id link
-}

forgetAllFromTo :: forall s. Id.NodeR -> LinksCmps s -> (LinksCmps s /\ List (LinkCmpState s))
forgetAllFromTo nodeR links =
    let
        connectedLinks :: List (LinkCmpState s)
        connectedLinks
            =  (links # Map.filterKeys (unwrap >>> _.from >>> Tuple.fst >>> (_ == nodeR)) # Map.values)
            <> (links # Map.filterKeys (unwrap >>> _.to   >>> Tuple.fst >>> (_ == nodeR)) # Map.values)
    in foldr forget links connectedLinks /\ connectedLinks


removeAllOf :: forall s m. K.PatchBoxKey -> List (LinkCmpState s) -> BlessedOp s m
removeAllOf pnk linksToRemove = do
    for_ linksToRemove $ flip remove pnk


findFrom :: forall s. Id.NodeR -> Id.OutletR -> LinksCmps s -> Maybe (LinkCmpState s)
findFrom nodeR outletR = Map.filter (unwrap >>> _.linkId >>> unwrap >>> _.from >>> (_ == nodeR /\ outletR)) >>> Map.values >>> List.head


findTo :: forall s. Id.NodeR -> Id.InletR -> LinksCmps s -> Maybe (LinkCmpState s)
findTo nodeR inletR = Map.filter (unwrap >>> _.linkId >>> unwrap >>> _.to >>> (_ == nodeR /\ inletR)) >>> Map.values >>> List.head
