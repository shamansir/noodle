module Web.Class.WebRenderer where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Tuple.Nested ((/\), type (/\))

import Type.Proxy (Proxy)
import Type.Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))

import Control.Monad.State (class MonadState)

import Noodle.Id (Family, FamilyR, NodeR, InletR) as Id
import Noodle.Node (Node)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Toolkit (ToolkitKey)
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Halogen (Component, RefLabel) as H
import Halogen.VDom.DOM.Prop (ElemRef)

import Front.Shared.Bounds (Position, Size, IntSize)
import Web.Components.ValueEditor (ValueEditor)
import Web.Components.ValueEditor (EditorId) as ValueEditor


class WebLocator :: Type -> Constraint
class WebLocator x where
    firstLocation :: x
    locateNext :: x -> Size -> x /\ Position
    -- defaultSize :: Proxy x -> Size


data ConstantShift
    = First
    | NextAfter Position


instance WebLocator ConstantShift where
    firstLocation :: ConstantShift
    firstLocation = First
    locateNext :: ConstantShift -> Size -> ConstantShift /\ Position
    locateNext (NextAfter { left, top }) _ =
        let
            nextPos =
                { left : 16.0 + left + 2.0
                , top : top + 30.0
                }
        in NextAfter nextPos /\ nextPos
    locateNext First _ = NextAfter { left : 16.0, top : 0.0 } /\ { left : 16.0, top : 0.0 }
    -- defaultSize :: Proxy PixelShift -> IntSize
    -- defaultSize _ = { width : 5, height : 2 }


class WebRenderer (tk :: ToolkitKey) (fs :: Families) repr m | tk -> fs where
    webSize :: forall (f :: Symbol) fstate is os. RegisteredFamily (F f fstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> H.RefLabel -> Node f fstate is os repr m -> Maybe IntSize
    renderWeb :: forall (f :: Symbol) fstate is os query input output. MonadEffect m => IsSymbol f => RegisteredFamily (F f fstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> H.RefLabel -> Node f fstate is os repr m -> Maybe (H.Component query input output m)


class WebRawRenderer (tk :: ToolkitKey) (fs :: Families) repr m | tk -> fs where
    webSizeRaw :: forall fstate. Proxy tk -> Proxy fs -> Id.FamilyR -> H.RefLabel -> Raw.Node fstate repr m -> Maybe IntSize
    renderWebRaw :: forall fstate query input output. Proxy tk -> Proxy fs -> Id.FamilyR -> H.RefLabel -> Raw.Node fstate repr m -> Maybe (H.Component query input output m)


type InletPath =
    { node :: Id.NodeR
    , inlet :: Id.InletR
    }


class WebEditor (tk :: ToolkitKey) repr m | tk -> repr where
    -- webEditorFor :: Proxy tk -> InletPath -> ValueInChannel repr -> Maybe ValueEditor.EditorId
    spawnWebEditor :: Proxy tk -> {- H.RefLabel -> -} ValueEditor.EditorId -> InletPath -> ValueInChannel repr -> Maybe (ValueEditor repr m)
