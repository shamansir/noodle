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
apply (AddNode patchId nodeDef) pushCmd nw =
    let nodePath = P.nodePath 0 0
    in
        Rpd.addNode' patchId nodeDef nw
            -- FIXME: `nodePath` should be real or/and just add `subscribeLastNode` method etc.
            -- FIXME: `onInletData`/`onOutletData` do not receive the proper state
            --        of the network this way, but they should (pass the current network
            --        state in the Process function?)
            </> Rpd.subscribeNode nodePath
                    (onNodeInletData nodePath)
                    (onNodeOutletData nodePath)
    where
        -- addModel = pure <<< ((/\) model)
        onNodeInletData nodePath (inletId /\ d) =
            pushCmd $ GotInletData (P.InletPath nodePath inletId) d
        onNodeOutletData nodePath maybeData =
            case maybeData of
                Just (outletId /\ d) ->
                    pushCmd $ GotOutletData (P.OutletPath nodePath outletId) d
                Nothing -> pure unit
apply _ _ nw = pure nw

