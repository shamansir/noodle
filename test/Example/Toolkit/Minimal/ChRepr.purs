module Example.Toolkit.Minimal.ChRepr where

import Prelude

import Partial.Unsafe (unsafePartial)

import Type.Proxy (Proxy)

import Data.Newtype (unwrap) as NT
-- import Data.String.Read (read)
import Data.Int (fromString, toStringAs, decimal) as Int
import Data.Maybe (Maybe(..), fromMaybe)

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ValueInChannel (class FromValueInChannel, class ToValueInChannel)
import Noodle.Repr.ValueInChannel (accept, decline) as ViC
import Noodle.Repr.Tagged (class ValueTagged, ValuePath) as VT
import Noodle.Raw.Fn.Shape (ValueTag, tagAs)

import Tidy.Codegen (exprCtor, exprIdent, exprInt, exprString, typeCtor)

import Noodle.Text.NdfFile.Types (EncodedType, EncodedValue(..))
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, pTypeFor, pDefaultFor, pValueFor, class ParseableRepr)

import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)


data MinimalVRepr
    = None
    | UnitV
    | Int Int
    | Str String


derive instance Eq MinimalVRepr


instance HasFallback MinimalVRepr where
    fallback = None


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



instance CodegenRepr MinimalVRepr where
    reprModule = const "Example.Toolkit.Minimal.ChRepr"
    reprTypeName = const "MinimalVRepr"
    reprType = const $ unsafePartial $ typeCtor "MinimalVRepr"
    pDefaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of
            Just "Int" -> exprInt 0
            Just "String" -> exprString ""
            Just "Unit" -> exprIdent "unit"
            _ -> exprCtor "None"
    pTypeFor = const $ unsafePartial $ \mbType ->
                  case NT.unwrap <$> mbType of
                    Just "Int" -> typeCtor "Int"
                    Just "String" -> typeCtor "String"
                    Just "Unit" -> typeCtor "Unit"
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