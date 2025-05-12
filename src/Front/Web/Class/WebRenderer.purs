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

import Web.Components.ValueEditor (ValueEditor)


class WebLocator :: Type -> Constraint
class WebLocator x where
    firstLocation :: x
    locateNext :: x -> { width :: Number, height :: Number } -> x /\ { left :: Number, top :: Number }
    -- defaultSize :: Proxy x -> { width :: Int, height :: Int }


data ConstantShift
    = First
    | NextAfter { left :: Number, top :: Number }


instance WebLocator ConstantShift where
    firstLocation :: ConstantShift
    firstLocation = First
    locateNext :: ConstantShift -> { width :: Number, height :: Number } -> ConstantShift /\ { left :: Number, top :: Number }
    locateNext (NextAfter { left, top }) _ =
        let
            nextPos =
                { left : 16.0 + left + 2.0
                , top : top + 30.0
                }
        in NextAfter nextPos /\ nextPos
    locateNext First _ = NextAfter { left : 16.0, top : 0.0 } /\ { left : 16.0, top : 0.0 }
    -- defaultSize :: Proxy PixelShift -> { width :: Int, height :: Int }
    -- defaultSize _ = { width : 5, height : 2 }


class WebRenderer (tk :: ToolkitKey) (fs :: Families) repr m | tk -> fs where
    webSize :: forall (f :: Symbol) fstate is os. RegisteredFamily (F f fstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> H.RefLabel -> Node f fstate is os repr m -> Maybe { width :: Int, height :: Int }
    renderWeb :: forall (f :: Symbol) fstate is os query input output. MonadEffect m => IsSymbol f => RegisteredFamily (F f fstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> H.RefLabel -> Node f fstate is os repr m -> Maybe (H.Component query input output m)


class WebRawRenderer (tk :: ToolkitKey) (fs :: Families) repr m | tk -> fs where
    webSizeRaw :: forall fstate. Proxy tk -> Proxy fs -> Id.FamilyR -> H.RefLabel -> Raw.Node fstate repr m -> Maybe { width :: Int, height :: Int }
    renderWebRaw :: forall fstate query input output. Proxy tk -> Proxy fs -> Id.FamilyR -> H.RefLabel -> Raw.Node fstate repr m -> Maybe (H.Component query input output m)


class WebEditor (tk :: ToolkitKey) repr | tk -> repr where
    webEditorFor :: Proxy tk -> Id.FamilyR -> H.RefLabel -> Id.NodeR {- Raw.Node fstate repr m -} -> Id.InletR -> ValueInChannel repr -> Maybe (ValueEditor repr Unit Effect)
