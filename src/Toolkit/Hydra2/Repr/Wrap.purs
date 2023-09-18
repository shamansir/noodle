module Toolkit.Hydra2.Repr.Wrap where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Repr as R -- (class ToRepr, class FromRepr, toRepr, fromRepr)
import Data.Mark (class Mark, mark)
import Data.String as String
import Data.String.Read (read)
import Data.FromToFile (class Encode, encode, class Decode, decode)
import Data.String.CodePoints as String
import Data.String.Read (class Read)
import Data.Number as Number

import Noodle.Node2.MapsFolds.Repr as NMF
import Noodle.Node2.Path (InNode)

import Toolkit.Hydra2.Types as H

data WrapRepr
    = Value H.Value
    | Unit Unit
    | Texture H.Texture
    | TOrV H.TOrV
    | TODO H.TODO
    | Context H.Context
    | UpdateFn H.UpdateFn
    | Source H.Source
    | Url H.Url
    | GlslFn H.GlslFn
    | SourceOptions H.SourceOptions
    | Values H.Values
    | Ease H.Ease
    | Audio H.AudioSource
    | AudioBin H.AudioBin
    | OutputN H.OutputN
    | SourceN H.SourceN
    | ExtSource H.ExtSource
    | Target H.RenderTarget
    | Fn H.Fn -- FIXME: review for excessive wraps, this one seems not to be needed, for example (we wrap Fn-s into values)
    | CBS H.CanBeSource


-- instance NMF.HasRepr a WrapRepr where
--     toRepr :: forall f i o. InNode f i o -> H.Value -> WrapRepr
--     toRepr _ a = WrapRepr


instance NMF.HasRepr Number WrapRepr where
    toRepr :: forall f i o. InNode f i o -> Number -> WrapRepr
    toRepr _ = H.Number >>> Value


instance NMF.HasRepr H.Value WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Value -> WrapRepr
    toRepr _ = Value


instance NMF.HasRepr Unit WrapRepr where
    toRepr :: forall f i o. InNode f i o -> Unit -> WrapRepr
    toRepr _ = Unit


instance NMF.HasRepr H.Texture WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Texture -> WrapRepr
    toRepr _ = Texture


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


instance NMF.HasRepr H.AudioSource WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.AudioSource -> WrapRepr
    toRepr _ = Audio


instance NMF.HasRepr H.AudioBin WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.AudioBin -> WrapRepr
    toRepr _ = AudioBin


instance NMF.HasRepr H.OutputN WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.OutputN -> WrapRepr
    toRepr _ = OutputN


instance NMF.HasRepr H.SourceN WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.SourceN -> WrapRepr
    toRepr _ = SourceN


instance NMF.HasRepr H.ExtSource WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.ExtSource -> WrapRepr
    toRepr _ = ExtSource


instance NMF.HasRepr H.RenderTarget WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.RenderTarget -> WrapRepr
    toRepr _ = Target


instance NMF.HasRepr H.Fn WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.Fn -> WrapRepr
    toRepr _ = H.Dep >>> Value


instance NMF.HasRepr H.CanBeSource WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.CanBeSource -> WrapRepr
    toRepr _ = CBS


instance NMF.HasRepr H.TOrV WrapRepr where
    toRepr :: forall f i o. InNode f i o -> H.TOrV -> WrapRepr
    toRepr _ = TOrV


instance NMF.HasRepr WrapRepr WrapRepr where
    toRepr :: forall f i o. InNode f i o -> WrapRepr -> WrapRepr
    toRepr _ = identity


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


instance R.ToRepr H.SourceN WrapRepr where
    toRepr :: H.SourceN -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< SourceN


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


instance R.ToRepr H.AudioSource WrapRepr where
    toRepr :: H.AudioSource -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Audio


instance R.ToRepr H.AudioBin WrapRepr where
    toRepr :: H.AudioBin -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< AudioBin


instance R.ToRepr H.OutputN WrapRepr where
    toRepr :: H.OutputN -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< OutputN


instance R.ToRepr H.ExtSource WrapRepr where
    toRepr :: H.ExtSource -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< ExtSource


instance R.ToRepr H.RenderTarget WrapRepr where
    toRepr :: H.RenderTarget -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Target


instance R.ToRepr H.Fn WrapRepr where
    toRepr :: H.Fn -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Value <<< H.Dep


instance R.ToRepr H.CanBeSource WrapRepr where
    toRepr :: H.CanBeSource -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< CBS


instance R.ToRepr H.TOrV WrapRepr where
    toRepr :: H.TOrV -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< TOrV


instance R.ToRepr WrapRepr WrapRepr where
    toRepr :: WrapRepr -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists


{- R.FromRepr -}


instance R.FromRepr WrapRepr H.Value where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Value
    fromRepr (R.Repr (Value value)) = Just value
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Texture where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Texture
    fromRepr (R.Repr (Texture texture)) = Just texture
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.OutputN where
    fromRepr :: R.Repr WrapRepr -> Maybe H.OutputN
    fromRepr (R.Repr (OutputN outN)) = Just outN
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.SourceN where
    fromRepr :: R.Repr WrapRepr -> Maybe H.SourceN
    fromRepr (R.Repr (SourceN srcN)) = Just srcN
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.TODO where
    fromRepr :: R.Repr WrapRepr -> Maybe H.TODO
    fromRepr (R.Repr (TODO todo)) = Just todo
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Context where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Context
    fromRepr (R.Repr (Context context)) = Just context
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.UpdateFn where
    fromRepr :: R.Repr WrapRepr -> Maybe H.UpdateFn
    fromRepr (R.Repr (UpdateFn updatefn)) = Just updatefn
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Source where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Source
    fromRepr (R.Repr (Source source)) = Just source
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Url where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Url
    fromRepr (R.Repr (Url url)) = Just url
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.GlslFn where
    fromRepr :: R.Repr WrapRepr -> Maybe H.GlslFn
    fromRepr (R.Repr (GlslFn glslfn)) = Just glslfn
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.SourceOptions where
    fromRepr :: R.Repr WrapRepr -> Maybe H.SourceOptions
    fromRepr (R.Repr (SourceOptions sourceoptions)) = Just sourceoptions
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Values where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Values
    fromRepr (R.Repr (Values values)) = Just values
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Ease where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Ease
    fromRepr (R.Repr (Ease ease)) = Just ease
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.AudioSource where
    fromRepr :: R.Repr WrapRepr -> Maybe H.AudioSource
    fromRepr (R.Repr (Audio audio)) = Just audio
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.AudioBin where
    fromRepr :: R.Repr WrapRepr -> Maybe H.AudioBin
    fromRepr (R.Repr (AudioBin audiobin)) = Just audiobin
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.ExtSource where
    fromRepr :: R.Repr WrapRepr -> Maybe H.ExtSource
    fromRepr (R.Repr (ExtSource ext)) = Just ext
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.RenderTarget where
    fromRepr :: R.Repr WrapRepr -> Maybe H.RenderTarget
    fromRepr (R.Repr (Target trg)) = Just trg
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.Fn where
    fromRepr :: R.Repr WrapRepr -> Maybe H.Fn
    fromRepr (R.Repr (Fn fn)) = Just fn
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.CanBeSource where
    fromRepr :: R.Repr WrapRepr -> Maybe H.CanBeSource
    fromRepr (R.Repr (CBS cbs)) = Just cbs
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr H.TOrV where
    fromRepr :: R.Repr WrapRepr -> Maybe H.TOrV
    fromRepr (R.Repr (Value v)) = Just $ H.V v
    fromRepr (R.Repr (Texture tex)) = Just $ H.T tex
    fromRepr (R.Repr (TOrV torv)) = Just torv
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr WrapRepr where
    fromRepr :: R.Repr WrapRepr -> Maybe WrapRepr
    fromRepr (R.Repr w) = Just w


instance Mark WrapRepr where
    mark = case _ of
        Value v -> mark v
        Unit unit -> mark unit
        Texture t -> mark t
        TOrV (H.T t) -> mark t
        TOrV (H.V v) -> mark v
        OutputN outN -> mark outN
        SourceN srcN -> mark srcN
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
        ExtSource ext -> mark ext
        Target trg -> mark trg
        Fn fn -> mark fn
        CBS cbs -> mark cbs


instance Show WrapRepr where
    show = case _ of
        Value v -> show v
        Unit unit -> show unit
        Texture t -> show t
        TOrV (H.T t) -> show t
        TOrV (H.V v) -> show v
        OutputN outN -> show outN
        SourceN srcN -> show srcN
        TODO todo -> show todo
        Context ctx -> show ctx
        UpdateFn fn -> show fn
        Source src -> show src
        Url url -> show url
        GlslFn fn -> show fn
        SourceOptions so -> show so
        Values vs -> show vs
        Ease e -> show e
        Audio a -> show a
        AudioBin ab -> show ab
        ExtSource ext -> show ext
        Target trg -> show trg
        Fn fn -> show fn
        CBS cbs -> show cbs

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


{-wrapParser :: Parser String WrapRepr
wrapParser =
    string "V" *> valueParser
    <|>
    string "U" *> pure unit -}


instance Encode WrapRepr where
    encode = case _ of
        Value v -> "V " <> encode v
        Unit _ -> "U U"
        Texture t -> "T " <> encode t
        TOrV (H.T t) -> "TT " <> encode t
        TOrV (H.V v) -> "VV" <> encode v
        OutputN outN -> "ON " <> encode outN
        SourceN srcN -> "SN " <> encode srcN
        TODO _ -> "TODO TODO"
        Context ctx -> "CTX " <> encode ctx
        UpdateFn fn -> "UFN " <> encode fn
        Source src -> "SRC " <> encode src
        Url url -> "URL " <> encode url
        GlslFn fn -> "GLSL " <> encode fn
        SourceOptions so -> "SO " <> encode so
        Values vs -> "VS " <> encode vs
        Ease e -> "E " <> encode e
        Audio a -> "A " <> encode a
        AudioBin ab -> "AB " <> encode ab
        ExtSource ext -> "EXT " <> encode ext
        Target trg -> "TRG " <> encode trg
        Fn fn -> "FN " <> encode fn
        CBS cbs -> "CBS " <> encode cbs


maybeEq :: WrapRepr -> WrapRepr -> Maybe Boolean
maybeEq a b = Just false


instance R.ReadRepr WrapRepr where
    readRepr :: String -> Maybe (R.Repr WrapRepr)
    readRepr s =
        if (String.take 4 s == "V N ") then
            case Number.fromString $ String.drop 4 s of
                Just n -> Just $ R.wrap $ Value $ H.Number n
                Nothing -> Nothing
        else Nothing


instance R.WriteRepr WrapRepr where
    writeRepr :: R.Repr WrapRepr -> String
    writeRepr = R.unwrap >>> encode
