module Hydra.Repr.Wrap where

import Prelude

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodePoints as String
import Data.Number as Number
import Data.Newtype (unwrap) as NT
import Data.Int (fromString) as Int
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either(..), either)
import Data.Bifunctor (bimap)

import Color as Color

import Parsing (Parser)
import Parsing.String as P
import Parsing.Extra (marker, foldMarkers, parseBy)

import Noodle.Repr as R
-- import Noodle.Node.MapsFolds.Repr as NMF
-- import Noodle.Node.Path (InNode)
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
import Noodle.Text.NdfFile.NodeDef.Codegen (class CodegenRepr, class ValueCodegen, mkExpression, familyPascalCase, groupPascalCase)
import Noodle.Text.NdfFile.NodeDef.Codegen (Options(..)) as CG
import Noodle.Text.NdfFile.Types (NodeFamily(..), FamilyGroup(..))
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)
import Noodle.Ui.Cli.Palette.Set.X11 (red2) as X11
import Noodle.Text.ToCode (class ToCode, toCode)
import Noodle.Text.FromCode (class CanParse, class FromCode, fromCode, fromParser, SourceError(..), Source, srcErrorToString)

import Hydra.Types as HT
import Hydra.Repr.Parser as RP
import Hydra.Repr.Target (HYDRA_V, hydraV)
import Hydra.Repr.Target (_encode) as H

import PureScript.CST.Types as CST
import Tidy.Codegen (exprCtor, exprIdent, exprInt, exprString, typeCtor, declImport, declImportAs, exprApp, exprRecord, exprArray)


-- import CompArts.Product as CAI


 -- FIXME: review for excessive wraps. Or maybe separate node state wraps and inputs/outputs wraps
 -- may be join classes IsNodeState/IsChannel with FromRepr/ToRepr and so on

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
--     toRepr :: forall f i o. InNode f i o -> HT.Value -> WrapRepr
--     toRepr _ a = WrapRepr


instance R.HasFallback WrapRepr where
    fallback = Unit unit


-- FIXME: There's a Generic class that is [almost] the same as `toRepr` / `FromRepr`: https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html


{-
instance NMF.HasRepr Number WrapRepr where
    toRepr :: forall f i o. InNode f i o -> Number -> WrapRepr
    toRepr _ = HT.Number >>> Value


instance NMF.HasRepr HT.Value WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.Value -> WrapRepr
    toRepr _ = Value


instance NMF.HasRepr Unit WrapRepr where
    toRepr :: forall f i o. InNode f i o -> Unit -> WrapRepr
    toRepr _ = Unit


instance NMF.HasRepr HT.Texture WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.Texture -> WrapRepr
    toRepr _ = Texture


instance NMF.HasRepr HT.TODO WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.TODO -> WrapRepr
    toRepr _ = TODO


instance NMF.HasRepr HT.Context WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.Context -> WrapRepr
    toRepr _ = Context


instance NMF.HasRepr HT.UpdateFn WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.UpdateFn -> WrapRepr
    toRepr _ = UpdateFn


instance NMF.HasRepr HT.Source WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.Source -> WrapRepr
    toRepr _ = Source


instance NMF.HasRepr HT.Url WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.Url -> WrapRepr
    toRepr _ = Url


instance NMF.HasRepr HT.GlslFn WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.GlslFn -> WrapRepr
    toRepr _ = GlslFn


instance NMF.HasRepr HT.SourceOptions WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.SourceOptions -> WrapRepr
    toRepr _ = SourceOptions


instance NMF.HasRepr HT.Values WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.Values -> WrapRepr
    toRepr _ = Values


instance NMF.HasRepr HT.Ease WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.Ease -> WrapRepr
    toRepr _ = Ease


instance NMF.HasRepr HT.AudioSource WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.AudioSource -> WrapRepr
    toRepr _ = Audio


instance NMF.HasRepr HT.AudioBin WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.AudioBin -> WrapRepr
    toRepr _ = AudioBin


instance NMF.HasRepr HT.OutputN WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.OutputN -> WrapRepr
    toRepr _ = OutputN


instance NMF.HasRepr HT.SourceN WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.SourceN -> WrapRepr
    toRepr _ = SourceN


instance NMF.HasRepr HT.ExtSource WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.ExtSource -> WrapRepr
    toRepr _ = ExtSource


instance NMF.HasRepr HT.RenderTarget WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.RenderTarget -> WrapRepr
    toRepr _ = Target


instance NMF.HasRepr HT.Fn WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.Fn -> WrapRepr
    toRepr _ = HT.Dep >>> Value


instance NMF.HasRepr HT.CanBeSource WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.CanBeSource -> WrapRepr
    toRepr _ = CBS


instance NMF.HasRepr HT.TOrV WrapRepr where
    toRepr :: forall f i o. InNode f i o -> HT.TOrV -> WrapRepr
    toRepr _ = TOrV


instance NMF.HasRepr CAI.Products WrapRepr where
    toRepr :: forall f i o. InNode f i o -> CAI.Products -> WrapRepr
    toRepr _ = Products


instance NMF.HasRepr CAI.Product' WrapRepr where
    toRepr :: forall f i o. InNode f i o -> CAI.Product' -> WrapRepr
    toRepr _ = Product


instance NMF.HasRepr WrapRepr WrapRepr where
    toRepr :: forall f i o. InNode f i o -> WrapRepr -> WrapRepr
    toRepr _ = identity
-}




{- R.ToRepr -}


instance R.ToRepr HT.Value WrapRepr where
    toRepr :: HT.Value -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Value


instance R.ToRepr Unit WrapRepr where
    toRepr :: Unit -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Unit


instance R.ToRepr HT.Texture WrapRepr where
    toRepr :: HT.Texture -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Texture


instance R.ToRepr HT.SourceN WrapRepr where
    toRepr :: HT.SourceN -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< SourceN


instance R.ToRepr HT.TODO WrapRepr where
    toRepr :: HT.TODO -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< TODO


instance R.ToRepr HT.Context WrapRepr where
    toRepr :: HT.Context -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Context


instance R.ToRepr HT.UpdateFn WrapRepr where
    toRepr :: HT.UpdateFn -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< UpdateFn


instance R.ToRepr HT.Source WrapRepr where
    toRepr :: HT.Source -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Source


instance R.ToRepr HT.Url WrapRepr where
    toRepr :: HT.Url -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Url


instance R.ToRepr HT.GlslFn WrapRepr where
    toRepr :: HT.GlslFn -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< GlslFn


instance R.ToRepr HT.SourceOptions WrapRepr where
    toRepr :: HT.SourceOptions -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< SourceOptions


instance R.ToRepr HT.Values WrapRepr where
    toRepr :: HT.Values -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Values


instance R.ToRepr HT.Ease WrapRepr where
    toRepr :: HT.Ease -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Ease


instance R.ToRepr HT.AudioSource WrapRepr where
    toRepr :: HT.AudioSource -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Audio


instance R.ToRepr HT.AudioBin WrapRepr where
    toRepr :: HT.AudioBin -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< AudioBin


instance R.ToRepr HT.OutputN WrapRepr where
    toRepr :: HT.OutputN -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< OutputN


instance R.ToRepr HT.ExtSource WrapRepr where
    toRepr :: HT.ExtSource -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< ExtSource


instance R.ToRepr HT.RenderTarget WrapRepr where
    toRepr :: HT.RenderTarget -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Target


instance R.ToRepr HT.DepFn WrapRepr where
    toRepr :: HT.DepFn -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Value <<< HT.Dep


instance R.ToRepr HT.CanBeSource WrapRepr where
    toRepr :: HT.CanBeSource -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< CBS


instance R.ToRepr HT.TOrV WrapRepr where
    toRepr :: HT.TOrV -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< TOrV


{-}
instance R.ToRepr CAI.Products WrapRepr where
    toRepr :: CAI.Products -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Products


instance R.ToRepr CAI.Product' WrapRepr where
    toRepr :: CAI.Product' -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists <<< Product
-}


instance R.ToRepr WrapRepr WrapRepr where
    toRepr :: WrapRepr -> Maybe (R.Repr WrapRepr)
    toRepr = R.exists


{- R.FromRepr -}


instance R.FromRepr WrapRepr HT.Value where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.Value
    fromRepr (R.Repr (Value value)) = Just value
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.Texture where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.Texture
    fromRepr (R.Repr (Texture texture)) = Just texture
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.OutputN where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.OutputN
    fromRepr (R.Repr (OutputN outN)) = Just outN
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.SourceN where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.SourceN
    fromRepr (R.Repr (SourceN srcN)) = Just srcN
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.TODO where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.TODO
    fromRepr (R.Repr (TODO todo)) = Just todo
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.Context where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.Context
    fromRepr (R.Repr (Context context)) = Just context
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.UpdateFn where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.UpdateFn
    fromRepr (R.Repr (UpdateFn updatefn)) = Just updatefn
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.Source where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.Source
    fromRepr (R.Repr (Source source)) = Just source
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.Url where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.Url
    fromRepr (R.Repr (Url url)) = Just url
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.GlslFn where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.GlslFn
    fromRepr (R.Repr (GlslFn glslfn)) = Just glslfn
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.SourceOptions where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.SourceOptions
    fromRepr (R.Repr (SourceOptions sourceoptions)) = Just sourceoptions
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.Values where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.Values
    fromRepr (R.Repr (Values values)) = Just values
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.Ease where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.Ease
    fromRepr (R.Repr (Ease ease)) = Just ease
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.AudioSource where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.AudioSource
    fromRepr (R.Repr (Audio audio)) = Just audio
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.AudioBin where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.AudioBin
    fromRepr (R.Repr (AudioBin audiobin)) = Just audiobin
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.ExtSource where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.ExtSource
    fromRepr (R.Repr (ExtSource ext)) = Just ext
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.RenderTarget where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.RenderTarget
    fromRepr (R.Repr (Target trg)) = Just trg
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.DepFn where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.DepFn
    fromRepr (R.Repr (DepFn fn)) = Just fn
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr HT.CanBeSource where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.CanBeSource
    fromRepr (R.Repr (CBS cbs)) = Just cbs
    fromRepr _ = Nothing


{-
instance R.FromRepr WrapRepr CAI.Products where
    fromRepr :: R.Repr WrapRepr -> Maybe CAI.Products
    fromRepr (R.Repr (Products products)) = Just products
    fromRepr _ = Nothing


instance R.FromRepr WrapRepr CAI.Product' where
    fromRepr :: R.Repr WrapRepr -> Maybe CAI.Product'
    fromRepr (R.Repr (Product product)) = Just product
    fromRepr _ = Nothing
-}

instance R.FromRepr WrapRepr HT.TOrV where
    fromRepr :: R.Repr WrapRepr -> Maybe HT.TOrV
    fromRepr (R.Repr (Value v)) = Just $ HT.V v
    fromRepr (R.Repr (Texture tex)) = Just $ HT.T tex
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


instance Show WrapRepr where
    show = case _ of
        Value v -> show v
        Unit unit -> show unit
        Texture t -> show t
        TOrV (HT.T t) -> show t
        TOrV (HT.V v) -> show v
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
        DepFn depFn -> show depFn
        -- Products products -> show products
        -- Product product -> show product
        CBS cbs -> show cbs
        WRError { source, error } -> "Error: \"" <> source <> "\" " <> error

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


hydraGenOptions :: CG.Options WrapRepr
hydraGenOptions =
    hydraGenOptions_
        { toModuleName : \fgroup family -> "Hydra" <> "." <> groupPascalCase fgroup <> "." <> familyPascalCase family
        , toInfoComment : \fgroup family -> "Generated by Noodle Codegen from Hydra NDF file."
        }


hydraGenOptions_ ::
    { toModuleName :: FamilyGroup -> NodeFamily -> String
    , toInfoComment :: FamilyGroup -> NodeFamily -> String
    } -> CG.Options WrapRepr
hydraGenOptions_ { toModuleName, toInfoComment } = CG.Options
  { temperamentAlgorithm : Temperament.defaultAlgorithm
  , monadAt : { module_ : "Effect", type_ : "Effect" }
  , nodeModuleName : toModuleName
  , prepr : (Proxy :: _ WrapRepr)
  , infoComment : Just toInfoComment
  , imports : unsafePartial $
    [ declImportAs "Hydra.Types" [ ] HT.hydraAlias_
    , declImportAs "Hydra.Repr.Wrap" [ ] wrapAlias_
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
                    "Audio" -> HT.hydraType_ "Audio"
                    "GlslFn" -> HT.hydraType_ "GlslFn"
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
            -- Just "Audio" -> tryMkExpression (Proxy :: _ HT.AudioSource)
            Just "GlslFn" -> tryMkExpression (Proxy :: _ HT.GlslFn)
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
            tryMkExpression _ (EncodedValue valueStr) = either (genError valueStr) mkExpression (fromCode hydraV unit valueStr :: Either SourceError res)
