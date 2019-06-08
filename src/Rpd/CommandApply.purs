module Rpd.CommandApply where

import Prelude (pure, Unit, unit, ($))
import Effect (Effect)

import Data.Maybe
import Data.Tuple.Nested ((/\))

import Rpd.Command (Command(..))
import Rpd.API as Rpd
import Rpd.API (Rpd, addPatch) as R
import Rpd.API ((</>))
import Rpd.Network (Network) as R
import Rpd.Process (InletHandler(..), OutletHandler(..), InletAlias) as R
import Rpd.Path as P
import Rpd.Toolkit as T


apply
    :: forall d c
     . Command d
    -> (Command d -> Effect Unit)
    -> T.Toolkit d c
    -> R.Network d
    -> R.Rpd (R.Network d)
apply Bang pushCmd _ nw =
    Rpd.subscribeAllInlets onInletData nw
        </> Rpd.subscribeAllOutlets onOutletData
    where
        onInletData inletPath d =
            pushCmd $ GotInletData inletPath d
        onOutletData outletPath d =
            pushCmd $ GotOutletData outletPath d
apply (AddPatch alias) pushCmd _ nw =
    R.addPatch alias nw
        -- FIXME: subscribe the nodes in the patch
apply (AddNode patchPath alias) pushCmd _ nw =
    Rpd.addNode patchPath alias nw
        -- FIXME: `onInletData`/`onOutletData` do not receive the proper state
        --        of the network this way (do they need it?), but they should
        --        (pass the current network state in the Process function?)
        </> Rpd.subscribeNode nodePath
                (onNodeInletData nodePath)
                (onNodeOutletData nodePath)
    where
        nodePath = P.nodeInPatch patchPath alias
        (patchAlias /\ nodeAlias) = P.explodeNodePath nodePath
        -- addModel = pure <<< ((/\) model)
        onNodeInletData nodePath (inletAlias /\ _ /\ d) =
            pushCmd $ GotInletData (P.toInlet patchAlias nodeAlias inletAlias) d
        onNodeOutletData nodePath (outletAlias /\ _ /\ d) =
            pushCmd $ GotOutletData (P.toOutlet patchAlias nodeAlias outletAlias) d
apply (AddInlet nodePath alias) pushCmd _ nw =
    let
        inletPath = P.inletInNode nodePath alias
        onInletData d =
            pushCmd $ GotInletData inletPath d
    in
        Rpd.addInlet nodePath alias nw
            </> Rpd.subscribeInlet inletPath (R.InletHandler onInletData)
apply (AddOutlet nodePath alias) pushCmd _ nw =
    let
        outletPath = P.outletInNode nodePath alias
        onOutletData d =
            pushCmd $ GotOutletData outletPath d
    in
        Rpd.addOutlet nodePath alias nw
            </> Rpd.subscribeOutlet outletPath (R.OutletHandler onOutletData)
apply (Connect { inlet : inletPath, outlet : outletPath }) _ _ nw =
    Rpd.connect outletPath inletPath nw
apply (Disconnect { inlet : inletPath, outlet : outletPath }) _ _ nw =
    Rpd.disconnectTop outletPath inletPath nw
-- TODO: Connect etc.
apply _ _ _ nw = pure nw

