module Example.Toolkit.Minimal.Repr where

import Prelude


import Data.Maybe (Maybe(..), fromMaybe)
import Noodle.Repr (class ToRepr, class FromRepr, class HasFallback)
import Noodle.Repr (wrap, unwrap) as Repr
import Data.Newtype (unwrap) as NT
-- import Data.String.Read (read)
import Data.Int (fromString) as Int

import Tidy.Codegen (exprCtor, exprIdent, exprInt, exprString, typeCtor)

import Partial.Unsafe (unsafePartial)

import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
import Noodle.Text.NdfFile.NodeDef.Codegen (class CodegenRepr)


data MinimalRepr
    = None
    | UnitV
    | Int Int
    | Str String


derive instance Eq MinimalRepr


instance Show MinimalRepr where
    show =
        case _ of
            None -> "<None>"
            Int n -> show n
            Str str -> str
            UnitV -> "<Unit>"
instance HasFallback MinimalRepr where
    fallback = None
instance ToRepr Int MinimalRepr where toRepr = Just <<< Repr.wrap <<< Int
instance ToRepr String MinimalRepr where toRepr = Just <<< Repr.wrap <<< Str
instance ToRepr Unit MinimalRepr where toRepr = Just <<< Repr.wrap <<< const UnitV
instance FromRepr MinimalRepr Int where
    fromRepr = Repr.unwrap >>>
        case _ of
            Int n -> Just n
            _ -> Nothing
instance FromRepr MinimalRepr String where
    fromRepr = Repr.unwrap >>>
        case _ of
            Str str -> Just str
            _ -> Nothing
instance FromRepr MinimalRepr Unit where
    fromRepr = Repr.unwrap >>>
        case _ of
            UnitV -> Just unit
            _ -> Nothing


instance CodegenRepr MinimalRepr where
    reprModule = const "Example.Toolkit.Minimal.Repr"
    reprTypeName = const "MinimalRepr"
    reprType = const $ unsafePartial $ typeCtor "MinimalRepr"
    defaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of
            Just "Int" -> exprInt 0
            Just "String" -> exprString ""
            Just "Unit" -> exprIdent "unit"
            _ -> exprCtor "None"
    typeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
                  case typeStr of
                    "Int" -> typeCtor "Int"
                    "String" -> typeCtor "String"
                    "Unit" -> typeCtor "Unit"
                    _ -> typeCtor "MinimalRepr"
    valueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
                  case NT.unwrap <$> mbType of
                     Just "Int" -> exprInt $ fromMaybe 0 $ Int.fromString valueStr
                     Just "String" -> exprString valueStr
                     Just "Unit" -> exprIdent "unit"
                     _ -> if (valueStr == "unit")
                                then exprIdent "unit"
                                else exprCtor "None"