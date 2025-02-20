module StarterTk.Simple.Metro where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Int (toNumber) as Int
import Demo.Toolkit.Starter.Repr.ChRepr (ValueRepr)
import Control.Monad.State as State
import Effect (Effect)
import Effect.Class (liftEffect)
import Noodle.Fn.Process as Fn
import Noodle.Fn.Process as Noodle
import Noodle.Fn.Shape (I, O)
import Noodle.Fn.Shape as Noodle
import Noodle.Fn.Shape.Temperament (Cold, Hot)
import Noodle.Id as NId
import Noodle.Node as Noodle
import Noodle.Toolkit.Families as Noodle
import Noodle.Toolkit.Family as Family
import Noodle.Toolkit.Family as Noodle
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Data.Tuple.Nested ((/\), type (/\))
import Noodle.Repr.HasFallback (class HasFallback)
import Demo.Toolkit.Starter.Repr.ChRepr as VR
import Signal (Signal, (~>))
import Signal (runSignal) as Signal
import Signal.Time (every) as Signal
import Data.Newtype (class Newtype, unwrap, wrap)

{- Generated by Noodle Codegen from NDF file. Group :: simple. Family :: metro.

[[  simple : metro :: <enabled:Bool {b/true} -> period:Time {t/2s}> => bang:Bang ]] (#9) -}

_metro :: NId.Family "metro"
_metro = NId.Family

type Inlets = (I "enabled" Hot Boolean :> I "period" Hot VR.Time :> TNil) :: Noodle.Inlets
type Outlets = (O "bang" VR.Bang :> TNil) :: Noodle.Outlets
type InletsRow = (enabled :: Boolean, period :: VR.Time)
type OutletsRow = (bang :: VR.Bang)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process State InletsRow OutletsRow ValueRepr Effect
type Node = Noodle.Node "metro" State InletsRow OutletsRow ValueRepr Effect
type Family = Noodle.Family "metro" State InletsRow OutletsRow ValueRepr Effect
type F = Noodle.F "metro" State InletsRow OutletsRow ValueRepr Effect

defaultI :: Record InletsRow
defaultI = { enabled: true, period: VR.Time { seconds: 0 } }

defaultO :: Record OutletsRow
defaultO = { bang: VR.Bang }

_in_enabled = Noodle.Inlet :: _ "enabled"
_in_period = Noodle.Inlet :: _ "period"
_out_bang = Noodle.Outlet :: _ "bang"

family :: Family
family = Family.make _metro initialState (Noodle.Shape :: Shape) defaultI defaultO metroP

makeNode :: Effect Node
makeNode = Family.spawn family

metroP :: Process
metroP = do
    enabled <- Noodle.receive _in_enabled
    period  <- Noodle.receive _in_period
    let seconds = case period of VR.Time { seconds } -> seconds
    curState <- State.get
    if enabled && seconds > 0 then
        case _.latest $ unwrap curState of
            Just _ -> pure unit
            Nothing -> do
                sendBang <- Fn.spawn $ do
                    Noodle.send _out_bang VR.Bang
                let genSignal = Signal.every (Int.toNumber seconds * 1000.0) ~> const sendBang
                State.modify_ $ unwrap >>> _ { latest = Just { period : VR.Time { seconds }, signal : genSignal } } >>> wrap
                Noodle.lift $ Signal.runSignal genSignal
    else pure unit


newtype State = State { latest :: Maybe { period :: VR.Time, signal :: Signal (Effect Unit) } }

derive instance Newtype State _

initialState = State { latest : Nothing } :: State

instance HasFallback State where
  fallback = initialState
