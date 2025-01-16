module Hydra.Repr.Wrap where

import Prelude

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Data.Maybe (Maybe(..))
import Data.String.CodePoints as String
import Data.Number as Number
import Data.Newtype (unwrap) as NT
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either, either)

import Color as Color

import Parsing (Parser)
import Parsing.String as P
import Parsing.Extra (marker, foldMarkers)

import Noodle.Repr.HasFallback as HF
import Noodle.Repr.StRepr as SR
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (class FromValueInChannel, class ToValueInChannel, accept, decline) as ViC
import Noodle.Repr.ChRepr (class ReadChannelRepr, class WriteChannelRepr) as CR
-- import Noodle.Node.MapsFolds.Repr as NMF
-- import Noodle.Node.Path (InNode)
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, class ValueCodegen, mkExpression, familyPascalCase, groupPascalCase, pDefaultFor, pValueFor)
import Noodle.Text.NdfFile.FamilyDef.Codegen (Options(..)) as FCG
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)
import Noodle.Text.ToCode (class ToCode)
import Noodle.Text.FromCode (class CanParse, class FromCode, fromCode, fromParser, SourceError, Source, srcErrorToString)

import Hydra.Types as HT
import Hydra.Repr.Parser as RP
import Hydra.Repr.Target (HYDRA_V, hydraV)
import Hydra.Repr.Target (_encode) as H
import Hydra.Repr.Show (class HydraShow, hShow)

import PureScript.CST.Types as CST
import Tidy.Codegen


-- import CompArts.Product as CAI


 -- FIXME: review for excessive wraps. Or maybe separate node state wraps and inputs/outputs wraps
 -- may be join classes IsNodeState/IsChannel with FromChRepr/ToChRepr and so on

data WrapRepr
    = Value HT.Value
    | Unit Unit
    | Texture HT.Texture
    | TOrV HT.TOrV
    | TODO HT.TODO
    | Context HT.Context
    | UpdateFn HT.UpdateFn
    | Source HT.Source
    | Url HT.Url
    | GlslFn HT.GlslFn
    | SourceOptions HT.SourceOptions
    | Values HT.Values
    | Ease HT.Ease
    | Audio HT.AudioSource
    | AudioBin HT.AudioBin
    | OutputN HT.OutputN
    | SourceN HT.SourceN
    | ExtSource HT.ExtSource
    | Target HT.RenderTarget
    | DepFn HT.DepFn -- for example this one seems not to be needed (we wrap Fn-s into values)
    | CBS HT.CanBeSource
    | WRError { source :: Source, error :: String }


-- instance NMF.HasRepr a WrapRepr where
--     fromValueInChannel :: forall f i o. InNode f i o -> HT.Value -> WrapRepr
--     fromValueInChannel _ a = WrapRepr


instance HF.HasFallback WrapRepr where
    fallback = Unit unit


{- ViC.FromValueInChannel -}


instance ViC.FromValueInChannel HT.Value WrapRepr where
    fromValueInChannel :: HT.Value -> WrapRepr
    fromValueInChannel = Value


instance ViC.FromValueInChannel Unit WrapRepr where
    fromValueInChannel :: Unit -> WrapRepr
    fromValueInChannel = Unit


instance ViC.FromValueInChannel HT.Texture WrapRepr where
    fromValueInChannel :: HT.Texture -> WrapRepr
    fromValueInChannel = Texture


instance ViC.FromValueInChannel HT.SourceN WrapRepr where
    fromValueInChannel :: HT.SourceN -> WrapRepr
    fromValueInChannel = SourceN


instance ViC.FromValueInChannel HT.TODO WrapRepr where
    fromValueInChannel :: HT.TODO -> WrapRepr
    fromValueInChannel = TODO


instance ViC.FromValueInChannel HT.Context WrapRepr where
    fromValueInChannel :: HT.Context -> WrapRepr
    fromValueInChannel = Context


instance ViC.FromValueInChannel HT.UpdateFn WrapRepr where
    fromValueInChannel :: HT.UpdateFn -> WrapRepr
    fromValueInChannel = UpdateFn


instance ViC.FromValueInChannel HT.Source WrapRepr where
    fromValueInChannel :: HT.Source -> WrapRepr
    fromValueInChannel = Source


instance ViC.FromValueInChannel HT.Url WrapRepr where
    fromValueInChannel :: HT.Url -> WrapRepr
    fromValueInChannel = Url


instance ViC.FromValueInChannel HT.GlslFn WrapRepr where
    fromValueInChannel :: HT.GlslFn -> WrapRepr
    fromValueInChannel = GlslFn


instance ViC.FromValueInChannel HT.SourceOptions WrapRepr where
    fromValueInChannel :: HT.SourceOptions -> WrapRepr
    fromValueInChannel = SourceOptions


instance ViC.FromValueInChannel HT.Values WrapRepr where
    fromValueInChannel :: HT.Values -> WrapRepr
    fromValueInChannel = Values


instance ViC.FromValueInChannel HT.Ease WrapRepr where
    fromValueInChannel :: HT.Ease -> WrapRepr
    fromValueInChannel = Ease


instance ViC.FromValueInChannel HT.AudioSource WrapRepr where
    fromValueInChannel :: HT.AudioSource -> WrapRepr
    fromValueInChannel = Audio


instance ViC.FromValueInChannel HT.AudioBin WrapRepr where
    fromValueInChannel :: HT.AudioBin -> WrapRepr
    fromValueInChannel = AudioBin


instance ViC.FromValueInChannel HT.OutputN WrapRepr where
    fromValueInChannel :: HT.OutputN -> WrapRepr
    fromValueInChannel = OutputN


instance ViC.FromValueInChannel HT.ExtSource WrapRepr where
    fromValueInChannel :: HT.ExtSource -> WrapRepr
    fromValueInChannel = ExtSource


instance ViC.FromValueInChannel HT.RenderTarget WrapRepr where
    fromValueInChannel :: HT.RenderTarget -> WrapRepr
    fromValueInChannel = Target


instance ViC.FromValueInChannel HT.DepFn WrapRepr where
    fromValueInChannel :: HT.DepFn -> WrapRepr
    fromValueInChannel = Value <<< HT.Dep


instance ViC.FromValueInChannel HT.CanBeSource WrapRepr where
    fromValueInChannel :: HT.CanBeSource -> WrapRepr
    fromValueInChannel = CBS


instance ViC.FromValueInChannel HT.TOrV WrapRepr where
    fromValueInChannel :: HT.TOrV -> WrapRepr
    fromValueInChannel = TOrV


{-}
instance ViC.FromValueInChannel CAI.Products WrapRepr where
    fromValueInChannel :: CAI.Products -> WrapRepr
    fromValueInChannel = Products


instance ViC.FromValueInChannel CAI.Product' WrapRepr where
    fromValueInChannel :: CAI.Product' -> WrapRepr
    fromValueInChannel = Product
-}


instance ViC.FromValueInChannel WrapRepr WrapRepr where
    fromValueInChannel :: WrapRepr -> WrapRepr
    fromValueInChannel = identity


{- ViC.ToValueInChannel -}


instance ViC.ToValueInChannel WrapRepr HT.Value where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.Value
    toValueInChannel (Value value) = ViC.accept value
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.Texture where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.Texture
    toValueInChannel (Texture texture) = ViC.accept texture
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.OutputN where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.OutputN
    toValueInChannel (OutputN outN) = ViC.accept outN
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.SourceN where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.SourceN
    toValueInChannel (SourceN srcN) = ViC.accept srcN
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.TODO where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.TODO
    toValueInChannel (TODO todo) = ViC.accept todo
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.Context where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.Context
    toValueInChannel (Context context) = ViC.accept context
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.UpdateFn where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.UpdateFn
    toValueInChannel (UpdateFn updatefn) = ViC.accept updatefn
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.Source where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.Source
    toValueInChannel (Source source) = ViC.accept source
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.Url where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.Url
    toValueInChannel (Url url) = ViC.accept url
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.GlslFn where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.GlslFn
    toValueInChannel (GlslFn glslfn) = ViC.accept glslfn
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.SourceOptions where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.SourceOptions
    toValueInChannel (SourceOptions sourceoptions) = ViC.accept sourceoptions
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.Values where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.Values
    toValueInChannel (Values values) = ViC.accept values
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.Ease where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.Ease
    toValueInChannel (Ease ease) = ViC.accept ease
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.AudioSource where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.AudioSource
    toValueInChannel (Audio audio) = ViC.accept audio
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.AudioBin where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.AudioBin
    toValueInChannel (AudioBin audiobin) = ViC.accept audiobin
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.ExtSource where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.ExtSource
    toValueInChannel (ExtSource ext) = ViC.accept ext
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.RenderTarget where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.RenderTarget
    toValueInChannel (Target trg) = ViC.accept trg
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.DepFn where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.DepFn
    toValueInChannel (DepFn fn) = ViC.accept fn
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr HT.CanBeSource where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.CanBeSource
    toValueInChannel (CBS cbs) = ViC.accept cbs
    toValueInChannel _ = ViC.decline


{-
instance ViC.ToValueInChannel WrapRepr CAI.Products where
    toValueInChannel :: WrapRepr -> ValueInChannel CAI.Products
    toValueInChannel (Products products) = ViC.accept products
    toValueInChannel _ = Nothing


instance ViC.ToValueInChannel WrapRepr CAI.Product' where
    toValueInChannel :: WrapRepr -> ValueInChannel CAI.Product'
    toValueInChannel (Product product) = ViC.accept product
    toValueInChannel _ = Nothing
-}

instance ViC.ToValueInChannel WrapRepr HT.TOrV where
    toValueInChannel :: WrapRepr -> ValueInChannel HT.TOrV
    toValueInChannel (Value v) = ViC.accept $ HT.V v
    toValueInChannel (Texture tex) = ViC.accept $ HT.T tex
    toValueInChannel (TOrV torv) = ViC.accept torv
    toValueInChannel _ = ViC.decline


instance ViC.ToValueInChannel WrapRepr WrapRepr where
    toValueInChannel :: WrapRepr -> ValueInChannel WrapRepr
    toValueInChannel = ViC.accept


instance Mark WrapRepr where
    mark = case _ of
        Value v -> mark v
        Unit unit -> mark unit
        Texture t -> mark t
        TOrV (HT.T t) -> mark t
        TOrV (HT.V v) -> mark v
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
        DepFn depFn -> mark depFn
        -- Products products -> mark unit -- FIXME: doesn't require Mark instance for node state wraps?
        -- Product product -> mark product
        CBS cbs -> mark cbs
        WRError _ -> Color.rgb 255 0 0


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



instance HydraShow WrapRepr where
    hShow = case _ of
        Value v -> hShow v
        Unit unit -> hShow unit
        Texture t -> hShow t
        TOrV (HT.T t) -> hShow t
        TOrV (HT.V v) -> hShow v
        OutputN outN -> hShow outN
        SourceN srcN -> hShow srcN
        TODO todo -> hShow todo
        Context ctx -> hShow ctx
        UpdateFn fn -> hShow fn
        Source src -> hShow src
        Url url -> hShow url
        GlslFn fn -> hShow fn
        SourceOptions so -> hShow so
        Values vs -> hShow vs
        Ease e -> hShow e
        Audio a -> hShow a
        AudioBin ab -> hShow ab
        ExtSource ext -> hShow ext
        Target trg -> hShow trg
        DepFn depFn -> hShow depFn
        -- Products products -> hShow products
        -- Product product -> hShow product
        CBS cbs -> hShow cbs
        WRError { source, error } -> "Error: \"" <> source <> "\" " <> error


wrapParser :: Parser String WrapRepr
wrapParser =
    foldMarkers
        [ marker $ "U" /\ Unit /\ (P.string "U" <#> const unit)
        , marker $ "TT" /\ TOrV <<< HT.T /\ RP.texture
        , marker $ "VV" /\ TOrV <<< HT.V /\ RP.value
        , marker $ "T" /\ Texture /\ RP.texture
        , marker $ "V" /\ Value /\ RP.value
        , marker $ "FN" /\ DepFn /\ RP.fn
        , marker $ "GLSL" /\ GlslFn /\ RP.glsl
        ]


{-wrapParser :: Parser String WrapRepr
wrapParser =
    string "V" *> valueParser
    <|>
    string "U" *> pure unit -}


instance ToCode HYDRA_V opts WrapRepr where
    toCode _ _ = case _ of
        Value v -> "V " <> H._encode v
        Unit _ -> "U U"
        Texture t -> "T " <> H._encode t
        TOrV (HT.T t) -> "TT " <> H._encode t
        TOrV (HT.V v) -> "VV" <> H._encode v
        OutputN outN -> "ON " <> H._encode outN
        SourceN srcN -> "SN " <> H._encode srcN
        TODO _ -> "TODO TODO"
        Context ctx -> "CTX " <> H._encode ctx
        UpdateFn fn -> "UFN " <> H._encode fn
        Source src -> "SRC " <> H._encode src
        Url url -> "URL " <> H._encode url
        GlslFn fn -> "GLSL " <> H._encode fn
        SourceOptions so -> "SO " <> H._encode so
        Values vs -> "VS " <> H._encode vs
        Ease e -> "E " <> H._encode e
        Audio a -> "A " <> H._encode a
        AudioBin ab -> "AB " <> H._encode ab
        ExtSource ext -> "EXT " <> H._encode ext
        Target trg -> "TRG " <> H._encode trg
        DepFn depFn -> "FN " <> H._encode depFn
        -- Products _ -> "PRS P"
        -- Product (CAI.Product' ix p) -> "PRD " <> show ix <> " " <> CAI.toUniqueId p
        CBS cbs -> "CBS " <> H._encode cbs
        WRError { source, error } -> "ERR \"" <> source <> "\" \"" <> error <> "\""


instance CanParse HYDRA_V WrapRepr where parser = const wrapParser
instance FromCode HYDRA_V opts WrapRepr where fromCode = fromParser


instance CR.ReadChannelRepr WrapRepr where
    readChannelRepr :: String -> Maybe WrapRepr
    readChannelRepr s =
        if (String.take 4 s == "V N ") then
            case Number.fromString $ String.drop 4 s of
                Just n -> Just $ Value $ HT.Number n
                Nothing -> Nothing
            -- FIXME: parse other kinds of reprs!
        else Nothing


instance CR.WriteChannelRepr WrapRepr where
    writeChannelRepr :: WrapRepr -> String
    writeChannelRepr = H._encode


wrapAlias_ = "HW" :: String
wrapPrefix_ = wrapAlias_ <> "." :: String
wrapTypeCtor_ :: Partial => String -> CST.Type Void
wrapTypeCtor_ s = typeCtor (wrapPrefix_ <> s)
wrapCtor_ :: Partial => String -> CST.Expr Void
wrapCtor_ s = exprCtor (wrapPrefix_ <> s)


hydraGenOptions :: FCG.Options WrapRepr WrapRepr
hydraGenOptions = FCG.Options
    { streprAt : { module_ : "Hydra.Repr.Wrap", type_ : "WrapRepr" }
    , chreprAt : { module_ : "Hydra.Repr.Wrap", type_ : "WrapRepr" }
    , temperamentAlgorithm : Temperament.defaultAlgorithm
    , monadAt : { module_ : "Effect", type_ : "Effect" }
    , familyModuleName : \fgroup family -> "Hydra" <> "." <> groupPascalCase fgroup <> "." <> familyPascalCase family
    , pstrepr : pWrap
    , pchrepr : pWrap
    , infoComment : Just $ \mbSource fgroup family ->
            "Generated by Noodle Codegen from Hydra NDF file. Group :: " <> show fgroup <> ". Family :: " <> show family <> "." <> case mbSource of
            Just src -> "\n\n[[ " <> src.line <> " ]] (#" <> show src.lineIndex <> ")"
            Nothing -> ""
    , tkImports : genericImports
    , familyImports : const genericImports
    }
    where
        genericImports = unsafePartial $
            [ declImportAs "Hydra.Types" [ ] HT.hydraAlias_
            , declImportAs "Hydra.Repr.Wrap" [ ] wrapAlias_
            , declImport "Noodle.Fn.ToFn" [ importTypeAll "Fn"]
            , declImport "Data.Tuple.Nested" [ importOp "/\\"]
            ]



pWrap = Proxy :: _ WrapRepr


instance CodegenRepr WrapRepr where
    reprModule :: Proxy WrapRepr -> String
    reprModule = const "Hydra.Repr.Wrap"
    reprTypeName :: Proxy WrapRepr -> String
    reprTypeName = const "WrapRepr"
    reprType :: Proxy WrapRepr -> CST.Type Void
    reprType = const $ unsafePartial $ wrapTypeCtor_ "WrapRepr"
    fTypeFor = const $ unsafePartial $ \_ -> typeCtor "WrapRepr"
    fDefaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of -- FIXME: use `HasFallback`
            Just "Value"   -> exprApp (wrapCtor_ "Value") [ pDefaultFor pWrap mbType ]
            Just "Texture" -> exprApp (wrapCtor_ "Texture") [ pDefaultFor pWrap mbType ]
            Just "TOrV"    -> exprApp (wrapCtor_ "TOrV") [ pDefaultFor pWrap mbType ]
            Just "TODO"    -> exprApp (wrapCtor_ "TODO") [ pDefaultFor pWrap mbType ]
            Just "Context" -> exprApp (wrapCtor_ "Context") [ pDefaultFor pWrap mbType ]
            Just "UpdateFn"-> exprApp (wrapCtor_ "UpdateFn") [ pDefaultFor pWrap mbType ]
            Just "Source"  -> exprApp (wrapCtor_ "Source") [ pDefaultFor pWrap mbType ]
            Just "Url"     -> exprApp (wrapCtor_ "Url") [ pDefaultFor pWrap mbType ]
            Just "GlslFn"  -> exprApp (wrapCtor_ "GlslFn") [ pDefaultFor pWrap mbType ]
            Just "SourceOptions" -> exprApp (wrapCtor_ "SourceOptions") [ pDefaultFor pWrap mbType ]
            Just "Values"  -> exprApp (wrapCtor_ "SourceOptions") [ pDefaultFor pWrap mbType ]
            Just "Ease"    -> exprApp (wrapCtor_ "Ease") [ pDefaultFor pWrap mbType ]
            Just "Audio"   -> exprApp (wrapCtor_ "Audio") [ pDefaultFor pWrap mbType ]
            Just "AudioBin" -> exprApp (wrapCtor_ "AudioBin") [ pDefaultFor pWrap mbType ]
            Just "OutputN" -> exprApp (wrapCtor_ "OutputN") [ pDefaultFor pWrap mbType ]
            Just "SourceN" -> exprApp (wrapCtor_ "SourceN") [ pDefaultFor pWrap mbType ]
            Just "ExtSource" -> exprApp (wrapCtor_ "ExtSource") [ pDefaultFor pWrap mbType ]
            Just "Target"  -> exprApp (wrapCtor_ "Target") [ pDefaultFor pWrap mbType ]
            Just "DepFn"   -> exprApp (wrapCtor_ "DepFn") [ pDefaultFor pWrap mbType ]
            Just "CBS"     -> exprApp (wrapCtor_ "CBS") [ pDefaultFor pWrap mbType ]
            _ -> wrapCtor_ "WrapRepr"
    fValueFor = const $ unsafePartial $ \mbType encV ->
        case NT.unwrap <$> mbType of -- FIXME: use `HasFallback`
            Just "Value"   -> exprApp (wrapCtor_ "Value") [ pValueFor pWrap mbType encV ]
            Just "Texture" -> exprApp (wrapCtor_ "Texture") [ pValueFor pWrap mbType encV ]
            Just "TOrV"    -> exprApp (wrapCtor_ "TOrV") [ pValueFor pWrap mbType encV ]
            Just "TODO"    -> exprApp (wrapCtor_ "TODO") [ pValueFor pWrap mbType encV ]
            Just "Context" -> exprApp (wrapCtor_ "Context") [ pValueFor pWrap mbType encV ]
            Just "UpdateFn"-> exprApp (wrapCtor_ "UpdateFn") [ pValueFor pWrap mbType encV ]
            Just "Source"  -> exprApp (wrapCtor_ "Source") [ pValueFor pWrap mbType encV ]
            Just "Url"     -> exprApp (wrapCtor_ "Url") [ pValueFor pWrap mbType encV ]
            Just "GlslFn"  -> exprApp (wrapCtor_ "GlslFn") [ pValueFor pWrap mbType encV ]
            Just "SourceOptions" -> exprApp (wrapCtor_ "SourceOptions") [ pValueFor pWrap mbType encV ]
            Just "Values"  -> exprApp (wrapCtor_ "SourceOptions") [ pValueFor pWrap mbType encV ]
            Just "Ease"    -> exprApp (wrapCtor_ "Ease") [ pValueFor pWrap mbType encV ]
            Just "Audio"   -> exprApp (wrapCtor_ "Audio") [ pValueFor pWrap mbType encV ]
            Just "AudioBin" -> exprApp (wrapCtor_ "AudioBin") [ pValueFor pWrap mbType encV ]
            Just "OutputN" -> exprApp (wrapCtor_ "OutputN") [ pValueFor pWrap mbType encV ]
            Just "SourceN" -> exprApp (wrapCtor_ "SourceN") [ pValueFor pWrap mbType encV ]
            Just "ExtSource" -> exprApp (wrapCtor_ "ExtSource") [ pValueFor pWrap mbType encV ]
            Just "Target"  -> exprApp (wrapCtor_ "Target") [ pValueFor pWrap mbType encV ]
            Just "DepFn"   -> exprApp (wrapCtor_ "DepFn") [ pValueFor pWrap mbType encV ]
            Just "CBS"     -> exprApp (wrapCtor_ "CBS") [ pValueFor pWrap mbType encV ]
            _ -> wrapCtor_ "WrapRepr"
    pTypeFor :: Proxy WrapRepr -> EncodedType -> CST.Type Void
    pTypeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
                case typeStr of
                    "Value" -> HT.hydraType_ "Value"
                    "Texture" -> HT.hydraType_ "Texture"
                    "TODO" -> HT.hydraType_ "TODO"
                    "Values" -> HT.hydraType_ "Values"
                    "Source" -> HT.hydraType_ "Source"
                    "GlslFn" -> HT.hydraType_ "GlslFn"
                    "OutputN" -> HT.hydraType_ "OutputN"
                    "Audio" -> HT.hydraType_ "AudioSource"
                    "RenderTarget" -> HT.hydraType_ "RenderTarget"
                    "SourceN" -> HT.hydraType_ "SourceN"
                    "SourceOptions" -> HT.hydraType_ "SourceOptions"
                    "Url" -> HT.hydraType_ "Url"
                    "UpdateFn" -> HT.hydraType_ "UpdateFn"
                    "Ease" -> HT.hydraType_ "Ease"
                    "AudioBin" -> HT.hydraType_ "AudioBin"
                    -- FIXME: implement further
                    _ -> wrapTypeCtor_ "WrapRepr"
    pDefaultFor :: Proxy WrapRepr -> Maybe EncodedType -> CST.Expr Void
    pDefaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of -- FIXME: use `HasFallback` from the repr somehow
            Just "Value" -> mkExpression (HF.fallback :: HT.Value)
            Just "Texture" -> mkExpression (HF.fallback :: HT.Texture)
            Just "TODO" -> mkExpression (HF.fallback :: HT.TODO)
            Just "Values" -> mkExpression (HF.fallback :: HT.Values)
            Just "Source" -> mkExpression (HF.fallback :: HT.Source)
            Just "GlslFn" -> mkExpression (HF.fallback :: HT.GlslFn)
            Just "OutputN" -> mkExpression (HF.fallback :: HT.OutputN)
            Just "Audio" -> mkExpression (HF.fallback :: HT.AudioSource)
            Just "RenderTarget" -> mkExpression (HF.fallback :: HT.RenderTarget)
            Just "SourceN" -> mkExpression (HF.fallback :: HT.SourceN)
            Just "SourceOptions" -> mkExpression (HF.fallback :: HT.SourceOptions)
            Just "Url" -> mkExpression (HF.fallback :: HT.Url)
            Just "UpdateFn" -> mkExpression (HF.fallback :: HT.UpdateFn)
            Just "Ease" -> mkExpression (HF.fallback :: HT.Ease)
            Just "AudioBin" -> mkExpression (HF.fallback :: HT.AudioBin)
            -- FIXME: implement further
            _ -> exprApp (wrapCtor_ "Value") [ HT.hydraCtor_ "None" ]
    pValueFor :: Proxy WrapRepr -> Maybe EncodedType -> EncodedValue -> CST.Expr Void
    pValueFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of
            Just "Value" -> tryMkExpression (Proxy :: _ HT.Value)
            Just "Texture" -> tryMkExpression (Proxy :: _ HT.Texture)
            Just "TODO" -> tryMkExpression (Proxy :: _ HT.TODO)
            Just "Values" -> tryMkExpression (Proxy :: _ HT.Values)
            Just "Source" -> tryMkExpression (Proxy :: _ HT.Source)
            Just "Audio" -> tryMkExpression (Proxy :: _ HT.AudioSource)
            Just "GlslFn" -> tryMkExpression (Proxy :: _ HT.GlslFn)
            Just "OutputN" -> tryMkExpression (Proxy :: _ HT.OutputN)
            Just "RenderTarget" -> tryMkExpression (Proxy :: _ HT.RenderTarget)
            Just "SourceN" -> tryMkExpression (Proxy :: _ HT.SourceN)
            Just "Ease" -> tryMkExpression (Proxy :: _ HT.Ease)
            Just "AudioBin" -> tryMkExpression (Proxy :: _ HT.AudioBin)
            -- Just "UpdateFn" -> tryMkExpression (Proxy :: _ HT.UpdateFn)
            -- Just "SourceOptions" -> tryMkExpression (Proxy :: _ HT.SourceOptions)
            -- Just "Url" -> tryMkExpression (Proxy :: _ HT.Url)
            -- FIXME: implement further
            _ -> const $ wrapCtor_ "Error"
        where
            genError :: Partial => Source -> SourceError -> CST.Expr Void
            genError src srcError =
                exprApp (exprCtor "Error")
                    [ exprRecord [ "source" /\ exprString src
                    , "error" /\ (exprString $ srcErrorToString srcError) ]
                    ]
            tryMkExpression :: forall res. Partial => ValueCodegen res => FromCode HYDRA_V Unit res => Proxy res -> EncodedValue -> CST.Expr Void
            tryMkExpression _ (EncodedValue valueStr) =
                either
                    (genError valueStr)
                    mkExpression
                    (fromCode hydraV unit valueStr :: Either SourceError res)
