module Toolkit.Hydra2.BlessedRepr where

import Prelude


import Noodle.Node2.MapsFolds.Repr as NMF
import Noodle.Node2.Path (InNode)

import Toolkit.Hydra2.Types as H


data BlessedRepr = BlessedRepr


instance NMF.HasRepr a BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> a -> BlessedRepr
    toRepr _ a = BlessedRepr