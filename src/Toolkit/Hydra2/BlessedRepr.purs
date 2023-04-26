module Toolkit.Hydra2.BlessedRepr where

import Prelude


import Noodle.Node2.MapsFolds.Repr as NMF
import Noodle.Node2.Path (InNode)

import Toolkit.Hydra2.Types as H


data BlessedRepr = BlessedRepr


-- instance NMF.HasRepr a BlessedRepr where
--     toRepr :: forall f i o. InNode f i o -> H.Value -> BlessedRepr
--     toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Value BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Value -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr Unit BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> Unit -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Texture BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Texture -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.From BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.From -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.TODO BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.TODO -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Context BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Context -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.UpdateFn BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.UpdateFn -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Source BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Source -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Url BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Url -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.GlslFn BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.GlslFn -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.SourceOptions BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.SourceOptions -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Values BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Values -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Ease BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Ease -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Audio BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Audio -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.AudioBin BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.AudioBin -> BlessedRepr
    toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Output BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Output -> BlessedRepr
    toRepr _ a = BlessedRepr