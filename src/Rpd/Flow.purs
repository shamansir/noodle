module Rpd.Flow
    ( FLOW
    , Flow
    , FlowEff, FlowEffE
    , subscribe, create, fold, sampleOn_
    , flow
    ) where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Eff (Eff, kind Effect)
import FRP (FRP)
import FRP.Event (class Filterable)
import FRP.Event as E
import FRP.Event.Class (class IsEvent)
import FRP.Event.Class as E
import Data.Filterable as F

foreign import data FLOW :: Effect

type FlowEffE e = (frp :: FRP, flow :: FLOW | e)
type FlowEff e v = Eff (FlowEffE e) v

type Flow d = E.Event d

subscribe
    :: forall d r eff
     . Flow d
    -> (d -> FlowEff eff r)
    -> FlowEff eff (FlowEff eff Unit)
subscribe = E.subscribe

create
    :: forall d eff
     . FlowEff eff
       { push :: d -> FlowEff eff Unit
       , flow :: Flow d
       }
create = do
    ePair <- E.create
    pure
        { push : ePair.push
        , flow : ePair.event
        }

--flow :: forall event d. (E.IsEvent event) => event d -> Flow d
flow :: forall d. E.Event d -> Flow d
flow = id


fold = E.fold


sampleOn_ = E.sampleOn_


-- instance flowIsEvent :: E.IsEvent Flow where
--     fold f (Flow e) i = Flow $ E.fold f e i
--     keepLatest (Flow flows) = Flow $ E.keepLatest $ map (\(Flow e) -> e) flows
--     sampleOn (Flow eA) (Flow eAtoB) = Flow $ E.sampleOn eA eAtoB
--     fix f = E.fix f
