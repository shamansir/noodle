module Rpd.CommandApply where

import Prelude (pure, Unit, unit, ($))
import Effect (Effect)

import Data.Maybe
import Data.Tuple.Nested ((/\))

import Rpd.Command (Command(..))
import Rpd.API as Rpd
import Rpd.API (Rpd, addPatch') as R
import Rpd.API ((</>))
import Rpd.Network (Network) as R
import Rpd.Process (InletHandler(..), OutletHandler(..)) as R
import Rpd.Path as P


apply
    :: forall d
     . Command d
    -> (Command d -> Effect Unit)
    -> R.Network d
    -> R.Rpd (R.Network d)
apply Bang pushCmd nw =
    Rpd.subscribeAllInlets onInletData nw
        </> Rpd.subscribeAllOutlets onOutletData
    where
        onInletData inletPath d =
            pushCmd $ GotInletData inletPath d
        onOutletData outletPath d =
            pushCmd $ GotOutletData outletPath d
apply (AddPatch patchDef) pushCmd nw =
    R.addPatch' patchDef nw
        -- FIXME: subscribe the nodes in the patch
apply (AddNode patchPath nodeDef) pushCmd nw =
    let nodePath = P.nodeInPatch patchPath 0 -- FIXME
    in
        Rpd.addNode' patchPath nodeDef nw
            -- FIXME: `nodePath` should be real or/and just add `subscribeLastNode` method etc.
            -- FIXME: `onInletData`/`onOutletData` do not receive the proper state
            --        of the network this way (do they need it?), but they should
            --        (pass the current network state in the Process function?)
            </> Rpd.subscribeNode nodePath
                    (onNodeInletData nodePath)
                    (onNodeOutletData nodePath)
    where
        -- addModel = pure <<< ((/\) model)
        onNodeInletData nodePath (inletId /\ d) =
            pushCmd $ GotInletData (P.inletInNode nodePath inletId) d
        onNodeOutletData nodePath maybeData =
            case maybeData of
                Just (outletId /\ d) ->
                    pushCmd $ GotOutletData (P.outletInNode nodePath outletId) d
                Nothing -> pure unit
apply (AddInlet nodePath inletDef) pushCmd nw =
    let
        inletId = 0
        inletPath = P.inletInNode nodePath inletId
        onInletData d =
            pushCmd $ GotInletData (P.inletInNode nodePath inletId) d
    in
        -- Rpd.makeId -> Effect String ??
        Rpd.addInlet' nodePath inletDef nw
            </> Rpd.subscribeInlet inletPath (R.InletHandler onInletData)
apply (AddOutlet nodePath outletDef) pushCmd nw =
    let
        outletId = 0
        outletPath = P.outletInNode nodePath outletId
        onOutletData d =
            pushCmd $ GotOutletData (P.outletInNode nodePath outletId) d
    in
        Rpd.addOutlet' nodePath outletDef nw
            </> Rpd.subscribeOutlet outletPath (R.OutletHandler onOutletData)
apply (Connect inletPath outletPath) _ nw =
    Rpd.connect inletPath outletPath nw
apply (Disconnect inletPath outletPath) _ nw =
    Rpd.disconnectTop inletPath outletPath nw
-- TODO: Connect etc.
apply _ _ nw = pure nw

