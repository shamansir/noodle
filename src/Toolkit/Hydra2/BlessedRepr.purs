module Toolkit.Hydra2.BlessedRepr where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Repr as R -- (class ToRepr, class FromRepr, toRepr, fromRepr)


import Noodle.Node2.MapsFolds.Repr as NMF
import Noodle.Node2.Path (InNode)

import Toolkit.Hydra2.Types as H


data BlessedRepr = BlessedRepr String


-- instance NMF.HasRepr a BlessedRepr where
--     toRepr :: forall f i o. InNode f i o -> H.Value -> BlessedRepr
--     toRepr _ a = BlessedRepr


instance NMF.HasRepr H.Value BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Value -> BlessedRepr
    toRepr _ a = BlessedRepr "Value"


instance NMF.HasRepr Unit BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> Unit -> BlessedRepr
    toRepr _ a = BlessedRepr "Unit"


instance NMF.HasRepr H.Texture BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Texture -> BlessedRepr
    toRepr _ a = BlessedRepr "Texture"


instance NMF.HasRepr H.From BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.From -> BlessedRepr
    toRepr _ a = BlessedRepr "From"


instance NMF.HasRepr H.TODO BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.TODO -> BlessedRepr
    toRepr _ a = BlessedRepr "TODO"


instance NMF.HasRepr H.Context BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Context -> BlessedRepr
    toRepr _ a = BlessedRepr "Context"


instance NMF.HasRepr H.UpdateFn BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.UpdateFn -> BlessedRepr
    toRepr _ a = BlessedRepr "UpdateFn"


instance NMF.HasRepr H.Source BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Source -> BlessedRepr
    toRepr _ a = BlessedRepr "Source"


instance NMF.HasRepr H.Url BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Url -> BlessedRepr
    toRepr _ a = BlessedRepr "Url"


instance NMF.HasRepr H.GlslFn BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.GlslFn -> BlessedRepr
    toRepr _ a = BlessedRepr "GlslFn"


instance NMF.HasRepr H.SourceOptions BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.SourceOptions -> BlessedRepr
    toRepr _ a = BlessedRepr "SourceOptions"


instance NMF.HasRepr H.Values BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Values -> BlessedRepr
    toRepr _ a = BlessedRepr "Values"


instance NMF.HasRepr H.Ease BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Ease -> BlessedRepr
    toRepr _ a = BlessedRepr "Ease"


instance NMF.HasRepr H.Audio BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Audio -> BlessedRepr
    toRepr _ a = BlessedRepr "Audio"


instance NMF.HasRepr H.AudioBin BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.AudioBin -> BlessedRepr
    toRepr _ a = BlessedRepr "AudioBin"


instance NMF.HasRepr H.Output BlessedRepr where
    toRepr :: forall f i o. InNode f i o -> H.Output -> BlessedRepr
    toRepr _ a = BlessedRepr "Output"


{- R.ToRepr -}


instance R.ToRepr H.Value BlessedRepr where
    toRepr :: H.Value -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Value")


instance R.ToRepr Unit BlessedRepr where
    toRepr :: Unit -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Unit")


instance R.ToRepr H.Texture BlessedRepr where
    toRepr :: H.Texture -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Texture")


instance R.ToRepr H.From BlessedRepr where
    toRepr :: H.From -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "From")


instance R.ToRepr H.TODO BlessedRepr where
    toRepr :: H.TODO -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "TODO")


instance R.ToRepr H.Context BlessedRepr where
    toRepr :: H.Context -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Context")


instance R.ToRepr H.UpdateFn BlessedRepr where
    toRepr :: H.UpdateFn -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "UpdateFn")


instance R.ToRepr H.Source BlessedRepr where
    toRepr :: H.Source -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Source")


instance R.ToRepr H.Url BlessedRepr where
    toRepr :: H.Url -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Url")


instance R.ToRepr H.GlslFn BlessedRepr where
    toRepr :: H.GlslFn -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "GlslFn")


instance R.ToRepr H.SourceOptions BlessedRepr where
    toRepr :: H.SourceOptions -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "SourceOptions")


instance R.ToRepr H.Values BlessedRepr where
    toRepr :: H.Values -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Values")


instance R.ToRepr H.Ease BlessedRepr where
    toRepr :: H.Ease -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Ease")


instance R.ToRepr H.Audio BlessedRepr where
    toRepr :: H.Audio -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Audio")


instance R.ToRepr H.AudioBin BlessedRepr where
    toRepr :: H.AudioBin -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "AudioBin")


instance R.ToRepr H.Output BlessedRepr where
    toRepr :: H.Output -> Maybe (R.Repr BlessedRepr)
    toRepr = R.exists <<< const (BlessedRepr "Output")


{- R.FromRepr -}


instance R.FromRepr BlessedRepr H.Value where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Value
    fromRepr (R.Repr (BlessedRepr "Value")) = Just H.None -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.Texture where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Texture
    fromRepr (R.Repr (BlessedRepr "Texture")) = Just H.Empty -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.From where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.From
    fromRepr (R.Repr (BlessedRepr "From")) = Just H.All -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.TODO where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.TODO
    fromRepr (R.Repr (BlessedRepr "TODO")) = Just H.TODO -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.Context where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Context
    fromRepr (R.Repr (BlessedRepr "Context")) = Just $ H.Context { time : 0.0 }  -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.UpdateFn where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.UpdateFn
    fromRepr (R.Repr (BlessedRepr "UpdateFn")) = Just H.defaultUpdateFn -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.Source where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Source
    fromRepr (R.Repr (BlessedRepr "Source")) = Just H.Dynamic -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.Url where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Url
    fromRepr (R.Repr (BlessedRepr "Url")) = Just H.noUrl -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.GlslFn where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.GlslFn
    fromRepr (R.Repr (BlessedRepr "GlslFn")) = Just H.defaultGlslFn -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.SourceOptions where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.SourceOptions
    fromRepr (R.Repr (BlessedRepr "SourceOptions")) = Just H.defaultSourceOptions -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.Values where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Values
    fromRepr (R.Repr (BlessedRepr "Values")) = Just H.noValues  -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.Ease where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Ease
    fromRepr (R.Repr (BlessedRepr "Ease")) = Just H.Linear -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.Audio where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Audio
    fromRepr (R.Repr (BlessedRepr "Audio")) = Just H.Silence -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.AudioBin where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.AudioBin
    fromRepr (R.Repr (BlessedRepr "AudioBin")) = Just H.H0 -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr BlessedRepr H.Output where
    fromRepr :: R.Repr BlessedRepr -> Maybe H.Output
    fromRepr (R.Repr (BlessedRepr "Output")) = Just H.Screen
    fromRepr _ = Nothing
