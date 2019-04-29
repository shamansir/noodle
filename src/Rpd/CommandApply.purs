module Rpd.CommandApply where

import Prelude (pure, Unit, unit, ($), (>>=))
import Effect (Effect)

import Data.Maybe
import Data.Tuple.Nested ((/\))

import Rpd.Command
import Rpd.API as Rpd
import Rpd.API as R
import Rpd.API ((</>))
import Rpd.Network as R
import Rpd.Path as P


-- apply :: forall d. Command d -> R.Network d -> R.Rpd (R.Network d)
-- apply (AddPatch patchDef) = R.addPatch' patchDef
-- apply (AddNode patchId nodeDef) = R.addNode' patchId nodeDef
-- apply _ = pure


apply
    :: forall d
     . Command d
    -> (CommandEffect d -> Effect Unit)
    -> R.Network d
    -- -> (Core.Message d -> R.Network d -> R.Rpd (R.Network d))
    -> R.Rpd (R.Network d)
apply Bang sendEffect nw =
    Rpd.subscribeAllInlets onInletData nw
        </> Rpd.subscribeAllOutlets onOutletData
    where
        onInletData inletPath d =
            sendEffect $ GotInletData inletPath d
        onOutletData outletPath d =
            sendEffect $ GotOutletData outletPath d
apply (AddPatch patchDef) sendEffect nw =
    R.addPatch' patchDef nw
        -- FIXME: subscribe the nodes in the patch
apply (AddNode patchId nodeDef) sendEffect nw =
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
            sendEffect $ GotInletData (P.InletPath nodePath inletId) d
        onNodeOutletData nodePath maybeData =
            case maybeData of
                Just (outletId /\ d) ->
                    sendEffect $ GotOutletData (P.OutletPath nodePath outletId) d
                Nothing -> pure unit
apply _ _ nw = pure nw

