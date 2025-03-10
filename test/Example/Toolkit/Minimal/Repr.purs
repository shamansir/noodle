module Example.Toolkit.Minimal.Repr where

import Prelude

import Partial.Unsafe (unsafePartial)

import Type.Proxy (Proxy)

import Data.Newtype (unwrap, wrap) as NT
-- import Data.String.Read (read)
import Data.Int (fromString, toStringAs, decimal) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple, uncurry, curry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class ReadChannelRepr, class WriteChannelRepr, readChannelRepr, writeChannelRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel, class FromValueInChannel, class ToValueInChannel)
import Noodle.Repr.ValueInChannel (accept, decline) as ViC
import Noodle.Repr.Tagged (class ValueTagged, ValuePath) as VT
import Noodle.Raw.Fn.Shape (ValueTag, tagAs)
import Noodle.Repr.StRepr (class StRepr)

import Tidy.Codegen (exprCtor, exprIdent, exprInt, exprString, exprOp, typeCtor, typeOp, binaryOp)

import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, pTypeFor, pDefaultFor, pValueFor, class ParseableRepr)

import Example.Toolkit.Minimal.PatchState (State(..)) as Patch

import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)


data MinimalVRepr
    = None
    | UnitV
    | Int Int
    | Str String


data MinimalStRepr
    = NoSt
    | UnitSt
    | StrSt String
    | PState ((Int /\ String) /\ String)


derive instance Eq MinimalVRepr


instance HasFallback MinimalVRepr where
    fallback = None


instance HasFallback MinimalStRepr where
    fallback = NoSt


instance FromValueInChannel MinimalVRepr MinimalVRepr where fromValueInChannel = identity
instance ToValueInChannel   MinimalVRepr MinimalVRepr where toValueInChannel   = ViC.accept

instance FromValueInChannel Int MinimalVRepr where fromValueInChannel = Int
instance FromValueInChannel String MinimalVRepr where fromValueInChannel = Str
instance FromValueInChannel Unit MinimalVRepr where fromValueInChannel = const UnitV


instance ToValueInChannel MinimalVRepr Int where
    toValueInChannel = case _ of
        Int n -> ViC.accept n
        _ -> ViC.decline
instance ToValueInChannel MinimalVRepr String where
    toValueInChannel = case _ of
        Str str -> ViC.accept str
        _ -> ViC.decline
instance ToValueInChannel MinimalVRepr Unit where
    toValueInChannel = case _ of
        UnitV -> ViC.accept unit
        _ -> ViC.decline


instance Show MinimalVRepr where
    show =
        case _ of
            None -> "<None>"
            Int n -> show n
            Str str -> str
            UnitV -> "<Unit>"


instance VT.ValueTagged MinimalVRepr where
    valueTag :: VT.ValuePath -> MinimalVRepr -> ValueTag
    valueTag = const $ tagAs <<< case _ of
        None -> "None"
        Int _ -> "Int"
        Str _ -> "Str"
        UnitV -> "Unit"
    acceptByTag :: Proxy MinimalVRepr -> { current :: ValueTag, incoming :: ValueTag } -> Boolean
    acceptByTag _ { current, incoming } = current == incoming


instance StRepr Unit MinimalStRepr where
    to = const UnitSt
    from = case _ of
        UnitSt -> Just unit
        _ -> Nothing
instance StRepr String MinimalStRepr where
    to = StrSt
    from = case _ of
        StrSt str -> Just str
        _ -> Nothing
instance StRepr (Tuple Patch.State String) MinimalStRepr where
    to :: Patch.State /\ String -> MinimalStRepr
    to = bimap (NT.unwrap >>> unpackP) identity >>> PState
        where unpackP { intVal, strVal } = intVal /\ strVal
    from :: MinimalStRepr -> Maybe (Patch.State /\ String)
    from = case _ of
        PState ((intVal /\ strVal) /\ nodeVal) -> Just $ (NT.wrap { intVal, strVal }) /\ nodeVal
        _ -> Nothing


instance CodegenRepr MinimalVRepr where
    reprModule = const "Example.Toolkit.Minimal.Repr"
    reprTypeName = const "MinimalVRepr"
    reprType = const $ unsafePartial $ typeCtor "MinimalVRepr"
    pDefaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of
            Just "Int" -> exprInt 0
            Just "String" -> exprString ""
            Just "Unit" -> exprIdent "unit"
            _ -> exprCtor "None"
    pTypeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
                  case typeStr of
                    "Int" -> typeCtor "Int"
                    "String" -> typeCtor "String"
                    "Unit" -> typeCtor "Unit"
                    _ -> typeCtor "MinimalVRepr"
    pValueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
                  case NT.unwrap <$> mbType of
                     Just "Int" -> exprInt $ fromMaybe 0 $ Int.fromString valueStr
                     Just "String" -> exprString valueStr
                     Just "Unit" -> exprIdent "unit"
                     _ -> if (valueStr == "unit")
                                then exprIdent "unit"
                                else exprCtor "None"
    fTypeFor prepr = pTypeFor prepr
    fDefaultFor prepr = pDefaultFor prepr
    fValueFor prepr = pValueFor prepr


instance CodegenRepr MinimalStRepr where
    reprModule = const "Example.Toolkit.Minimal.Repr"
    reprTypeName = const "MinimalStRepr"
    reprType = const $ unsafePartial $ typeCtor "MinimalStRepr"
    pDefaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of
            Just "UnitSt" -> exprIdent "unit"
            Just "StrSt" -> exprString ""
            Just "PState" ->
                exprOp (exprInt 0)
                    [ binaryOp "/\\" $ exprString ""
                    , binaryOp "/\\" $ exprString ""
                    ]
            _ -> exprCtor "NoSt"
    pTypeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
                  case typeStr of
                    "StrSt" -> typeCtor "String"
                    "UnitSt" -> typeCtor "Unit"
                    "PState" ->
                        typeOp (typeCtor "Int")
                            [ binaryOp "/\\" $ typeCtor "String"
                            , binaryOp "/\\" $ typeCtor "String"
                            ]
                    _ -> typeCtor "MinimalStRepr"
    pValueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
                  case NT.unwrap <$> mbType of
                     Just "StrSt" -> exprString valueStr
                     Just "UnitSt" -> exprIdent "unit"
                     Just "PState" -> exprIdent "foo" -- FIXME:
                     _ -> if (valueStr == "unit")
                                then exprCtor "NoSt" --exprIdent "unit"
                                else exprCtor "NoSt"
    fTypeFor prepr = pTypeFor prepr
    fDefaultFor prepr = pDefaultFor prepr
    fValueFor prepr = pValueFor prepr


instance ParseableRepr MinimalVRepr where
    toDefault = toDefaultImpl
    toRepr = toReprImpl


toDefaultImpl :: EncodedType -> MinimalVRepr
toDefaultImpl = NT.unwrap >>> case _ of
    "None"    -> None
    "Unit"    -> UnitV
    "Int"     -> Int 0
    "String"  -> Str ""
    _ -> None


toReprImpl :: EncodedType -> EncodedValue -> Maybe MinimalVRepr
toReprImpl eType eValue =
    case NT.unwrap eType of
        "None"    -> Just None
        "Unit"    -> Just UnitV
        "Int"     -> Int <$> (Int.fromString $ NT.unwrap eValue)
        "String"  -> Just $ Str $ NT.unwrap eValue
        _ -> Nothing


instance ReadForeign MinimalVRepr where -- FIXME: use `Taggable` & `ParseableRepr`
    readImpl fgn = do
        (rec :: { tag :: String, value :: String }) <- readImpl fgn
        pure $ case rec.tag of
            "None" -> None
            "Unit" -> UnitV
            "Int" -> fromMaybe None $ Int <$> Int.fromString rec.value
            "String" -> Str rec.value
            _ -> None


instance WriteForeign MinimalVRepr where -- FIXME: use `Taggable` & `ParseableRepr`
    writeImpl = case _ of
        None -> writeImpl { tag : "None", value : "" }
        UnitV -> writeImpl { tag : "Unit", value : "" }
        Int val -> writeImpl { tag : "Int", value : Int.toStringAs Int.decimal val }
        Str str -> writeImpl { tag : "String", value : str }