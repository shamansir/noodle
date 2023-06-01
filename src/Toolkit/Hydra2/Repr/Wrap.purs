module Toolkit.Hydra2.Repr.Wrap where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Repr as R -- (class ToRepr, class FromRepr, toRepr, fromRepr)
import Data.Mark (class Mark, mark)

import Noodle.Node2.MapsFolds.Repr as NMF
import Noodle.Node2.Path (InNode)

import Toolkit.Hydra2.Types as H


data WrapRepr
    = Value H.Value
    | Unit Unit
    | Texture H.Texture
    | From H.From
    | TODO H.TODO
    | Context H.Context
    | UpdateFn H.UpdateFn
    | Source H.Source
    | Url H.Url
    | GlslFn H.GlslFn
    | SourceOptions H.SourceOptions
    | Values H.Values
    | Ease H.Ease
    | Audio H.Audio
    | AudioBin H.AudioBin
    | Output H.Output


-- instance NMF.HasRepr a WrapRepr where
--     toRepr :: forall f i o. InNode f i o -> H.Value -> WrapRepr
--     toRepr _ a = WrapRepr


instance NMF.HasRepr H.Value WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Value -> WrapRepr
    toRepr _ = Value


instance NMF.HasRepr Unit WrapRepr where
    toRepr :: forall f i o. InNode f i o -> Unit -> WrapRepr
    toRepr _ = Unit


instance NMF.HasRepr H.Texture WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Texture -> WrapRepr
    toRepr _ = Texture


instance NMF.HasRepr H.From WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.From -> WrapRepr
    toRepr _ = From


instance NMF.HasRepr H.TODO WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.TODO -> WrapRepr
    toRepr _ = TODO


instance NMF.HasRepr H.Context WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Context -> WrapRepr
    toRepr _ = Context


instance NMF.HasRepr H.UpdateFn WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.UpdateFn -> WrapRepr
    toRepr _ = UpdateFn


instance NMF.HasRepr H.Source WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Source -> WrapRepr
    toRepr _ = Source


instance NMF.HasRepr H.Url WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Url -> WrapRepr
    toRepr _ = Url


instance NMF.HasRepr H.GlslFn WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.GlslFn -> WrapRepr
    toRepr _ = GlslFn


instance NMF.HasRepr H.SourceOptions WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.SourceOptions -> WrapRepr
    toRepr _ = SourceOptions


instance NMF.HasRepr H.Values WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Values -> WrapRepr
    toRepr _ = Values


instance NMF.HasRepr H.Ease WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Ease -> WrapRepr
    toRepr _ = Ease


instance NMF.HasRepr H.Audio WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Audio -> WrapRepr
    toRepr _ = Audio


instance NMF.HasRepr H.AudioBin WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.AudioBin -> WrapRepr
    toRepr _ = AudioBin


instance NMF.HasRepr H.Output WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Output -> WrapRepr
    toRepr _ = Output


{- R.ToRepr -}


instance R.ToRepr H.Value WrapRepr where
    toRepr :: H.Value -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Value


instance R.ToRepr Unit WrapRepr where
    toRepr :: Unit -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Unit


instance R.ToRepr H.Texture WrapRepr where
    toRepr :: H.Texture -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Texture


instance R.ToRepr H.From WrapRepr where
    toRepr :: H.From -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< From


instance R.ToRepr H.TODO WrapRepr where
    toRepr :: H.TODO -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< TODO


instance R.ToRepr H.Context WrapRepr where
    toRepr :: H.Context -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Context


instance R.ToRepr H.UpdateFn WrapRepr where
    toRepr :: H.UpdateFn -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< UpdateFn


instance R.ToRepr H.Source WrapRepr where
    toRepr :: H.Source -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Source


instance R.ToRepr H.Url WrapRepr where
    toRepr :: H.Url -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Url


instance R.ToRepr H.GlslFn WrapRepr where
    toRepr :: H.GlslFn -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< GlslFn


instance R.ToRepr H.SourceOptions WrapRepr where
    toRepr :: H.SourceOptions -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< SourceOptions


instance R.ToRepr H.Values WrapRepr where
    toRepr :: H.Values -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Values


instance R.ToRepr H.Ease WrapRepr where
    toRepr :: H.Ease -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Ease


instance R.ToRepr H.Audio WrapRepr where
    toRepr :: H.Audio -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Audio


instance R.ToRepr H.AudioBin WrapRepr where
    toRepr :: H.AudioBin -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< AudioBin


instance R.ToRepr H.Output WrapRepr where
    toRepr :: H.Output -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Output


{- R.FromRepr -}


instance R.FromRepr WrapRepr H.Value where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Value
    fromRepr (R.Repr (Value value)) = Just value -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Texture where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Texture
    fromRepr (R.Repr (Texture texture)) = Just texture -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.From where
    fromRepr :: R.Repr WrapRepr -> Maybe H.From
    fromRepr (R.Repr (From from)) = Just from -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.TODO where
    fromRepr :: R.Repr WrapRepr -> Maybe H.TODO
    fromRepr (R.Repr (TODO todo)) = Just todo -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Context where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Context
    fromRepr (R.Repr (Context context)) = Just context
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.UpdateFn where
    fromRepr :: R.Repr WrapRepr -> Maybe H.UpdateFn
    fromRepr (R.Repr (UpdateFn updatefn)) = Just updatefn -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Source where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Source
    fromRepr (R.Repr (Source source)) = Just source -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Url where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Url
    fromRepr (R.Repr (Url url)) = Just url -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.GlslFn where
    fromRepr :: R.Repr WrapRepr -> Maybe H.GlslFn
    fromRepr (R.Repr (GlslFn glslfn)) = Just glslfn -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.SourceOptions where
    fromRepr :: R.Repr WrapRepr -> Maybe H.SourceOptions
    fromRepr (R.Repr (SourceOptions sourceoptions)) = Just sourceoptions -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Values where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Values
    fromRepr (R.Repr (Values values)) = Just values  -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Ease where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Ease
    fromRepr (R.Repr (Ease ease)) = Just ease -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Audio where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Audio
    fromRepr (R.Repr (Audio audio)) = Just audio -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.AudioBin where
    fromRepr :: R.Repr WrapRepr -> Maybe H.AudioBin
    fromRepr (R.Repr (AudioBin audiobin)) = Just audiobin -- FIXME
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Output where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Output
    fromRepr (R.Repr (Output output)) = Just output
    fromRepr _ = Nothing


instance Mark WrapRepr where
    mark = case _ of
        Value v -> mark v
        Unit unit -> mark unit
        Texture t -> mark t
        From from -> mark from
        TODO todo -> mark todo
        Context ctx -> mark ctx
        UpdateFn fn -> mark fn
        Source src -> mark src
        Url url -> mark url
        GlslFn fn -> mark fn
        SourceOptions so -> mark so
        Values vs -> mark vs
        Ease e -> mark e
        Audio a -> mark a
        AudioBin ab -> mark ab
        Output o -> mark o

    {-
    mark = case _ of
        Value _ -> X11.lightyellow -- X11.seagreen-- mark HG.Synth
        Unit _ -> X11.lightgray
        Texture _ -> X11.darkorange
        From _ -> X11.limegreen
        TODO _ -> X11.burlywood
        Context _ -> X11.papayawhip
        UpdateFn _ -> X11.salmon
        Source _ -> X11.cornsilk
        Url _ -> X11.cornflowerblue
        GlslFn _ -> X11.crimson
        SourceOptions _ -> X11.palevioletred
        Values _ -> mark HG.Array
        Ease _ -> X11.darkgoldenrod
        Audio _ -> mark HG.Audio
        AudioBin _ -> X11.aqua
        Output _ -> X11.blue
    -}