module Toolkit.Hydra2.Repr.Text where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Repr as R -- (class ToRepr, class FromRepr, toRepr, fromRepr)


import Noodle.Node2.MapsFolds.Repr as NMF
import Noodle.Node2.Path (InNode)

import Toolkit.Hydra2.Types as H


data TextRepr = TextRepr String


-- instance NMF.HasRepr a TextRepr where
--     toRepr :: forall f i o. InNode f i o -> H.Value -> TextRepr
--     toRepr _ a = TextRepr


instance NMF.HasRepr H.Value TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Value -> TextRepr
    toRepr _ a = TextRepr "Value"


instance NMF.HasRepr Unit TextRepr where
    toRepr :: forall f i o. InNode f i o -> Unit -> TextRepr
    toRepr _ a = TextRepr "Unit"


instance NMF.HasRepr H.Texture TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Texture -> TextRepr
    toRepr _ a = TextRepr "Texture"


instance NMF.HasRepr H.From TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.From -> TextRepr
    toRepr _ a = TextRepr "From"


instance NMF.HasRepr H.TODO TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.TODO -> TextRepr
    toRepr _ a = TextRepr "TODO"


instance NMF.HasRepr H.Context TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Context -> TextRepr
    toRepr _ a = TextRepr "Context"


instance NMF.HasRepr H.UpdateFn TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.UpdateFn -> TextRepr
    toRepr _ a = TextRepr "UpdateFn"


instance NMF.HasRepr H.Source TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Source -> TextRepr
    toRepr _ a = TextRepr "Source"


instance NMF.HasRepr H.Url TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Url -> TextRepr
    toRepr _ a = TextRepr "Url"


instance NMF.HasRepr H.GlslFn TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.GlslFn -> TextRepr
    toRepr _ a = TextRepr "GlslFn"


instance NMF.HasRepr H.SourceOptions TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.SourceOptions -> TextRepr
    toRepr _ a = TextRepr "SourceOptions"


instance NMF.HasRepr H.Values TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Values -> TextRepr
    toRepr _ a = TextRepr "Values"


instance NMF.HasRepr H.Ease TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Ease -> TextRepr
    toRepr _ a = TextRepr "Ease"


instance NMF.HasRepr H.Audio TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Audio -> TextRepr
    toRepr _ a = TextRepr "Audio"


instance NMF.HasRepr H.AudioBin TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.AudioBin -> TextRepr
    toRepr _ a = TextRepr "AudioBin"


instance NMF.HasRepr H.Output TextRepr where
    toRepr :: forall f i o. InNode f i o -> H.Output -> TextRepr
    toRepr _ a = TextRepr "Output"


{- R.ToRepr -}


instance R.ToRepr H.Value TextRepr where
    toRepr :: H.Value -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Value")


instance R.ToRepr Unit TextRepr where
    toRepr :: Unit -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Unit")


instance R.ToRepr H.Texture TextRepr where
    toRepr :: H.Texture -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Texture")


instance R.ToRepr H.From TextRepr where
    toRepr :: H.From -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "From")


instance R.ToRepr H.TODO TextRepr where
    toRepr :: H.TODO -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "TODO")


instance R.ToRepr H.Context TextRepr where
    toRepr :: H.Context -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Context")


instance R.ToRepr H.UpdateFn TextRepr where
    toRepr :: H.UpdateFn -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "UpdateFn")


instance R.ToRepr H.Source TextRepr where
    toRepr :: H.Source -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Source")


instance R.ToRepr H.Url TextRepr where
    toRepr :: H.Url -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Url")


instance R.ToRepr H.GlslFn TextRepr where
    toRepr :: H.GlslFn -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "GlslFn")


instance R.ToRepr H.SourceOptions TextRepr where
    toRepr :: H.SourceOptions -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "SourceOptions")


instance R.ToRepr H.Values TextRepr where
    toRepr :: H.Values -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Values")


instance R.ToRepr H.Ease TextRepr where
    toRepr :: H.Ease -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Ease")


instance R.ToRepr H.Audio TextRepr where
    toRepr :: H.Audio -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Audio")


instance R.ToRepr H.AudioBin TextRepr where
    toRepr :: H.AudioBin -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "AudioBin")


instance R.ToRepr H.Output TextRepr where
    toRepr :: H.Output -> Maybe (R.Repr TextRepr)
    toRepr = R.exists <<< const (TextRepr "Output")


{- R.FromRepr -}


instance R.FromRepr TextRepr H.Value where
    fromRepr :: R.Repr TextRepr -> Maybe H.Value
    fromRepr (R.Repr (TextRepr "Value")) = Just H.None -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.Texture where
    fromRepr :: R.Repr TextRepr -> Maybe H.Texture
    fromRepr (R.Repr (TextRepr "Texture")) = Just H.Empty -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.From where
    fromRepr :: R.Repr TextRepr -> Maybe H.From
    fromRepr (R.Repr (TextRepr "From")) = Just H.All -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.TODO where
    fromRepr :: R.Repr TextRepr -> Maybe H.TODO
    fromRepr (R.Repr (TextRepr "TODO")) = Just H.TODO -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.Context where
    fromRepr :: R.Repr TextRepr -> Maybe H.Context
    fromRepr (R.Repr (TextRepr "Context")) = Just $ H.Context { time : 0.0, mouseX : 0.0, mouseY : 0.0 }  -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.UpdateFn where
    fromRepr :: R.Repr TextRepr -> Maybe H.UpdateFn
    fromRepr (R.Repr (TextRepr "UpdateFn")) = Just H.defaultUpdateFn -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.Source where
    fromRepr :: R.Repr TextRepr -> Maybe H.Source
    fromRepr (R.Repr (TextRepr "Source")) = Just H.Dynamic -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.Url where
    fromRepr :: R.Repr TextRepr -> Maybe H.Url
    fromRepr (R.Repr (TextRepr "Url")) = Just H.noUrl -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.GlslFn where
    fromRepr :: R.Repr TextRepr -> Maybe H.GlslFn
    fromRepr (R.Repr (TextRepr "GlslFn")) = Just H.defaultGlslFn -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.SourceOptions where
    fromRepr :: R.Repr TextRepr -> Maybe H.SourceOptions
    fromRepr (R.Repr (TextRepr "SourceOptions")) = Just H.defaultSourceOptions -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.Values where
    fromRepr :: R.Repr TextRepr -> Maybe H.Values
    fromRepr (R.Repr (TextRepr "Values")) = Just H.noValues  -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.Ease where
    fromRepr :: R.Repr TextRepr -> Maybe H.Ease
    fromRepr (R.Repr (TextRepr "Ease")) = Just H.Linear -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.Audio where
    fromRepr :: R.Repr TextRepr -> Maybe H.Audio
    fromRepr (R.Repr (TextRepr "Audio")) = Just H.Silence -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.AudioBin where
    fromRepr :: R.Repr TextRepr -> Maybe H.AudioBin
    fromRepr (R.Repr (TextRepr "AudioBin")) = Just H.H0 -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr TextRepr H.Output where
    fromRepr :: R.Repr TextRepr -> Maybe H.Output
    fromRepr (R.Repr (TextRepr "Output")) = Just H.Screen
    fromRepr _ = Nothing
