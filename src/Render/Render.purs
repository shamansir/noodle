module Render
    ( UI(..)
    , UIState(..)
    , Event(..), Selection(..)
    , UIChannel
    , init, update
    ) where

import Prelude

import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Rpd as R
import Signal as S
import Signal.Channel as SC


newtype UIState d =
    UIState
        { selection :: Selection
        , dragging :: Maybe R.NodePath
        , connecting :: Maybe R.OutletPath
        , curDataMsg :: Maybe (R.DataMsg d)
        , lastInletData :: Map R.InletPath d
        , lastOutletData :: Map R.OutletPath d
        }


data Event d
    = Start
    | Skip
    | ConnectFrom R.OutletPath
    | ConnectTo R.InletPath
    | Drag Int Int
    | Data (R.DataMsg d)
    | Select Selection


data Selection
    = SNone
    | SNetwork -- a.k.a. None ?
    | SPatch R.PatchId
    | SNode R.NodePath
    | SInlet R.InletPath
    | SOutlet R.InletPath
    | SLink R.LinkId


data UI d = UI (UIState d) (R.Network d)


type UIChannel d = SC.Channel (Event d)


init :: forall d. UIState d
init =
    UIState
        { selection : SNone
        , dragging : Nothing
        , connecting : Nothing
        , curDataMsg : Nothing
        , lastInletData : Map.empty
        , lastOutletData : Map.empty
        }


update :: forall d e. Event d -> UI d -> UI d
update (Data dataMsg) (UI (UIState state) network) =
    let
        curDataMsg = Just dataMsg
    in
        UI
            (UIState $
                state { curDataMsg = Just dataMsg
                      , lastInletData =
                            case dataMsg of
                                R.FromInlet inletPath d ->
                                    Map.insert inletPath d state.lastInletData
                                _ -> state.lastInletData
                      , lastOutletData =
                            case dataMsg of
                                R.FromOutlet outletPath d ->
                                    Map.insert outletPath d state.lastOutletData
                                _ -> state.lastOutletData
                      })
            network
update _ ui = ui
