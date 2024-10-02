module Test.MyToolkit.Repr where

import Prelude


import Data.Maybe (Maybe(..), fromMaybe)
import Data.Repr (class ToRepr, class FromRepr, class HasFallback)
import Data.Repr (wrap, unwrap) as Repr
import Data.Newtype (unwrap) as NT
-- import Data.String.Read (read)
import Data.Int (fromString) as Int

import Tidy.Codegen hiding (importType, importTypeOp, importValue, importTypeAll)
import Tidy.Codegen.Monad

import Partial.Unsafe (unsafePartial)

import Noodle.Text.NdfFile.NodeDef.Codegen (class CodegenRepr)
import Noodle.Text.NdfFile.Types


data ISRepr
    = None
    | UnitV
    | Int Int
    | Str String


derive instance Eq ISRepr


instance Show ISRepr where
    show =
        case _ of
            None -> "<None>"
            Int n -> show n
            Str str -> str
            UnitV -> "<Unit>"
instance HasFallback ISRepr where
    fallback = None
instance ToRepr Int ISRepr where toRepr = Just <<< Repr.wrap <<< Int
instance ToRepr String ISRepr where toRepr = Just <<< Repr.wrap <<< Str
instance ToRepr Unit ISRepr where toRepr = Just <<< Repr.wrap <<< const UnitV
instance FromRepr ISRepr Int where
    fromRepr = Repr.unwrap >>>
        case _ of
            Int n -> Just n
            _ -> Nothing
instance FromRepr ISRepr String where
    fromRepr = Repr.unwrap >>>
        case _ of
            Str str -> Just str
            _ -> Nothing
instance FromRepr ISRepr Unit where
    fromRepr = Repr.unwrap >>>
        case _ of
            UnitV -> Just unit
            _ -> Nothing


instance CodegenRepr ISRepr where
    reprModule = const "Test.MyToolkit.Repr"
    reprTypeName = const "ISRepr"
    reprType = const $ unsafePartial $ typeCtor "ISRepr"
    reprDefault = const $ unsafePartial $ exprCtor "None"
    typeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
                  case typeStr of
                    "Int" -> typeCtor "Int"
                    "String" -> typeCtor "String"
                    "Unit" -> typeCtor "Unit"
                    _ -> typeCtor "ISRepr"
    valueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
                  case NT.unwrap <$> mbType of
                     Just "Int" -> exprInt $ fromMaybe 0 $ Int.fromString valueStr
                     Just "String" -> exprString valueStr
                     Just "Unit" -> exprIdent "unit"
                     _ -> if (valueStr == "unit")
                                then exprIdent "unit"
                                else exprCtor "None"