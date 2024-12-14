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

import Noodle.Repr.ChRepr as CR
-- import Noodle.Node.MapsFolds.Repr as NMF
-- import Noodle.Node.Path (InNode)
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, class ValueCodegen, mkExpression, familyPascalCase, groupPascalCase)
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
--     toChRepr :: forall f i o. InNode f i o -> HT.Value -> WrapRepr
--     toChRepr _ a = WrapRepr


instance HF.HasFallback WrapRepr where
    fallback = Unit unit


{- CR.ToChRepr -}


instance CR.ToChRepr HT.Value WrapRepr where
    toChRepr :: HT.Value -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Value


instance CR.ToChRepr Unit WrapRepr where
    toChRepr :: Unit -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Unit


instance CR.ToChRepr HT.Texture WrapRepr where
    toChRepr :: HT.Texture -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Texture


instance CR.ToChRepr HT.SourceN WrapRepr where
    toChRepr :: HT.SourceN -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< SourceN


instance CR.ToChRepr HT.TODO WrapRepr where
    toChRepr :: HT.TODO -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< TODO


instance CR.ToChRepr HT.Context WrapRepr where
    toChRepr :: HT.Context -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Context


instance CR.ToChRepr HT.UpdateFn WrapRepr where
    toChRepr :: HT.UpdateFn -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< UpdateFn


instance CR.ToChRepr HT.Source WrapRepr where
    toChRepr :: HT.Source -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Source


instance CR.ToChRepr HT.Url WrapRepr where
    toChRepr :: HT.Url -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Url


instance CR.ToChRepr HT.GlslFn WrapRepr where
    toChRepr :: HT.GlslFn -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< GlslFn


instance CR.ToChRepr HT.SourceOptions WrapRepr where
    toChRepr :: HT.SourceOptions -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< SourceOptions


instance CR.ToChRepr HT.Values WrapRepr where
    toChRepr :: HT.Values -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Values


instance CR.ToChRepr HT.Ease WrapRepr where
    toChRepr :: HT.Ease -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Ease


instance CR.ToChRepr HT.AudioSource WrapRepr where
    toChRepr :: HT.AudioSource -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Audio


instance CR.ToChRepr HT.AudioBin WrapRepr where
    toChRepr :: HT.AudioBin -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< AudioBin


instance CR.ToChRepr HT.OutputN WrapRepr where
    toChRepr :: HT.OutputN -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< OutputN


instance CR.ToChRepr HT.ExtSource WrapRepr where
    toChRepr :: HT.ExtSource -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< ExtSource


instance CR.ToChRepr HT.RenderTarget WrapRepr where
    toChRepr :: HT.RenderTarget -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Target


instance CR.ToChRepr HT.DepFn WrapRepr where
    toChRepr :: HT.DepFn -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Value <<< HT.Dep


instance CR.ToChRepr HT.CanBeSource WrapRepr where
    toChRepr :: HT.CanBeSource -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< CBS


instance CR.ToChRepr HT.TOrV WrapRepr where
    toChRepr :: HT.TOrV -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< TOrV


{-}
instance CR.ToChRepr CAI.Products WrapRepr where
    toChRepr :: CAI.Products -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Products


instance CR.ToChRepr CAI.Product' WrapRepr where
    toChRepr :: CAI.Product' -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists <<< Product
-}


instance CR.ToChRepr WrapRepr WrapRepr where
    toChRepr :: WrapRepr -> Maybe (R.Repr WrapRepr)
    toChRepr = R.exists


{- CR.FromChRepr -}


instance CR.FromChRepr WrapRepr HT.Value where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.Value
    fromChRepr (R.Repr (Value value)) = Just value
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.Texture where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.Texture
    fromChRepr (R.Repr (Texture texture)) = Just texture
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.OutputN where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.OutputN
    fromChRepr (R.Repr (OutputN outN)) = Just outN
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.SourceN where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.SourceN
    fromChRepr (R.Repr (SourceN srcN)) = Just srcN
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.TODO where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.TODO
    fromChRepr (R.Repr (TODO todo)) = Just todo
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.Context where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.Context
    fromChRepr (R.Repr (Context context)) = Just context
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.UpdateFn where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.UpdateFn
    fromChRepr (R.Repr (UpdateFn updatefn)) = Just updatefn
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.Source where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.Source
    fromChRepr (R.Repr (Source source)) = Just source
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.Url where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.Url
    fromChRepr (R.Repr (Url url)) = Just url
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.GlslFn where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.GlslFn
    fromChRepr (R.Repr (GlslFn glslfn)) = Just glslfn
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.SourceOptions where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.SourceOptions
    fromChRepr (R.Repr (SourceOptions sourceoptions)) = Just sourceoptions
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.Values where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.Values
    fromChRepr (R.Repr (Values values)) = Just values
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.Ease where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.Ease
    fromChRepr (R.Repr (Ease ease)) = Just ease
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.AudioSource where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.AudioSource
    fromChRepr (R.Repr (Audio audio)) = Just audio
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.AudioBin where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.AudioBin
    fromChRepr (R.Repr (AudioBin audiobin)) = Just audiobin
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.ExtSource where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.ExtSource
    fromChRepr (R.Repr (ExtSource ext)) = Just ext
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.RenderTarget where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.RenderTarget
    fromChRepr (R.Repr (Target trg)) = Just trg
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.DepFn where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.DepFn
    fromChRepr (R.Repr (DepFn fn)) = Just fn
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr HT.CanBeSource where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.CanBeSource
    fromChRepr (R.Repr (CBS cbs)) = Just cbs
    fromChRepr _ = Nothing


{-
instance CR.FromChRepr WrapRepr CAI.Products where
    fromChRepr :: R.Repr WrapRepr -> Maybe CAI.Products
    fromChRepr (R.Repr (Products products)) = Just products
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr CAI.Product' where
    fromChRepr :: R.Repr WrapRepr -> Maybe CAI.Product'
    fromChRepr (R.Repr (Product product)) = Just product
    fromChRepr _ = Nothing
-}

instance CR.FromChRepr WrapRepr HT.TOrV where
    fromChRepr :: R.Repr WrapRepr -> Maybe HT.TOrV
    fromChRepr (R.Repr (Value v)) = Just $ HT.V v
    fromChRepr (R.Repr (Texture tex)) = Just $ HT.T tex
    fromChRepr (R.Repr (TOrV torv)) = Just torv
    fromChRepr _ = Nothing


instance CR.FromChRepr WrapRepr WrapRepr where
    fromChRepr :: R.Repr WrapRepr -> Maybe WrapRepr
    fromChRepr (R.Repr w) = Just w


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


instance R.ReadRepr WrapRepr where
    readRepr :: String -> Maybe (R.Repr WrapRepr)
    readRepr s =
        if (String.take 4 s == "V N ") then
            case Number.fromString $ String.drop 4 s of
                Just n -> Just $ R.wrap $ Value $ HT.Number n
                Nothing -> Nothing
            -- FIXME: parse other kinds of reprs!
        else Nothing


instance R.WriteRepr WrapRepr where
    writeRepr :: R.Repr WrapRepr -> String
    writeRepr = R.unwrap >>> H._encode


wrapAlias_ = "HW" :: String
wrapPrefix_ = wrapAlias_ <> "." :: String
wrapTypeCtor_ :: Partial => String -> CST.Type Void
wrapTypeCtor_ s = typeCtor (wrapPrefix_ <> s)
wrapCtor_ :: Partial => String -> CST.Expr Void
wrapCtor_ s = exprCtor (wrapPrefix_ <> s)


hydraGenOptions :: FCG.Options WrapRepr
hydraGenOptions = FCG.Options
    { reprAt : { module_ : "Hydra.Repr.Wrap", type_ : "WrapRepr" }
    , temperamentAlgorithm : Temperament.defaultAlgorithm
    , monadAt : { module_ : "Effect", type_ : "Effect" }
    , familyModuleName : \fgroup family -> "Hydra" <> "." <> groupPascalCase fgroup <> "." <> familyPascalCase family
    , prepr : (Proxy :: _ WrapRepr)
    , infoComment : Just $ \mbSource fgroup family ->
            "Generated by Noodle Codegen from Hydra NDF file. Group :: " <> show fgroup <> ". Family :: " <> show family <> "." <> case mbSource of
            Just src -> "\n\n[[ " <> src.line <> " ]] (#" <> show src.lineIndex <> ")"
            Nothing -> ""
    , imports : unsafePartial $
        [ declImportAs "Hydra.Types" [ ] HT.hydraAlias_
        , declImportAs "Hydra.Repr.Wrap" [ ] wrapAlias_
        , declImport "Noodle.Fn.ToFn" [ importTypeAll "Fn"]
        , declImport "Data.Tuple.Nested" [ importOp "/\\"]
        ]
    }


instance CodegenRepr WrapRepr where
    reprModule :: Proxy WrapRepr -> String
    reprModule = const "Hydra.Repr.Wrap"
    reprTypeName :: Proxy WrapRepr -> String
    reprTypeName = const "WrapRepr"
    reprType :: Proxy WrapRepr -> CST.Type Void
    reprType = const $ unsafePartial $ wrapTypeCtor_ "WrapRepr"
    typeFor :: Proxy WrapRepr -> EncodedType -> CST.Type Void
    typeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
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
    defaultFor :: Proxy WrapRepr -> Maybe EncodedType -> CST.Expr Void
    defaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of -- FIXME: use `HasFallback` from the repr somehow
            Just "Value" -> mkExpression (R.fallback :: HT.Value)
            Just "Texture" -> mkExpression (R.fallback :: HT.Texture)
            Just "TODO" -> mkExpression (R.fallback :: HT.TODO)
            Just "Values" -> mkExpression (R.fallback :: HT.Values)
            Just "Source" -> mkExpression (R.fallback :: HT.Source)
            Just "GlslFn" -> mkExpression (R.fallback :: HT.GlslFn)
            Just "OutputN" -> mkExpression (R.fallback :: HT.OutputN)
            Just "Audio" -> mkExpression (R.fallback :: HT.AudioSource)
            Just "RenderTarget" -> mkExpression (R.fallback :: HT.RenderTarget)
            Just "SourceN" -> mkExpression (R.fallback :: HT.SourceN)
            Just "SourceOptions" -> mkExpression (R.fallback :: HT.SourceOptions)
            Just "Url" -> mkExpression (R.fallback :: HT.Url)
            Just "UpdateFn" -> mkExpression (R.fallback :: HT.UpdateFn)
            Just "Ease" -> mkExpression (R.fallback :: HT.Ease)
            Just "AudioBin" -> mkExpression (R.fallback :: HT.AudioBin)
            -- FIXME: implement further
            _ -> exprApp (wrapCtor_ "Value") [ HT.hydraCtor_ "None" ]
    valueFor :: Proxy WrapRepr -> Maybe EncodedType -> EncodedValue -> CST.Expr Void
    valueFor = const $ unsafePartial $ \mbType ->
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
