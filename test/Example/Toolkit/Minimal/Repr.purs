module Example.Toolkit.Minimal.Repr where

import Prelude


import Data.Maybe (Maybe(..), fromMaybe)
import Noodle.Repr (class ToRepr, class FromRepr, class HasFallback, toRepr, fromRepr, Repr(..))
import Noodle.Repr (fromEq, toEq, wrap, unwrap, ensureTo, ensureFrom) as Repr
import Data.Newtype (unwrap) as NT
-- import Data.String.Read (read)
import Data.Int (fromString) as Int
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))

import Tidy.Codegen (exprCtor, exprIdent, exprInt, exprString, typeCtor)

import Partial.Unsafe (unsafePartial)

import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr)


import Example.Toolkit.Minimal.PatchState (State(..)) as Patch


data MinimalRepr
    = None
    | UnitV
    | Int Int
    | Str String
    | Tup MinimalRepr MinimalRepr -- FIXME: Store just Int & String for Patch instead, no overcomplication


derive instance Eq MinimalRepr


instance FromRepr MinimalRepr MinimalRepr where fromRepr = Repr.fromEq
instance ToRepr   MinimalRepr MinimalRepr where toRepr   = Repr.toEq


instance Show MinimalRepr where
    show =
        case _ of
            None -> "<None>"
            Int n -> show n
            Str str -> str
            UnitV -> "<Unit>"
            Tup reprA reprB -> show reprA <> " /\\ " <> show reprB


instance HasFallback MinimalRepr where
    fallback = None


instance ToRepr Int MinimalRepr where toRepr = Just <<< Repr.wrap <<< Int
instance ToRepr String MinimalRepr where toRepr = Just <<< Repr.wrap <<< Str
instance ToRepr Unit MinimalRepr where toRepr = Just <<< Repr.wrap <<< const UnitV

instance ToRepr Patch.State MinimalRepr where
    toRepr = NT.unwrap >>> \{ intVal, strVal } -> toRepr $ intVal /\ strVal

instance
    ( ToRepr a MinimalRepr
    , ToRepr b MinimalRepr
    )
    => ToRepr (Tuple a b) MinimalRepr where
    toRepr = Just <<< Repr.wrap <<<
        \(a /\ b) ->
            Tup
                (Repr.unwrap $ Repr.ensureTo a)
                (Repr.unwrap $ Repr.ensureTo b)


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
instance FromRepr MinimalRepr Patch.State where
    fromRepr = Repr.unwrap >>>
        case _ of
            Tup (Int intVal) (Str strVal) -> Just $ Patch.State { intVal, strVal }
            _ -> Nothing
instance (FromRepr MinimalRepr a, FromRepr MinimalRepr b) => FromRepr MinimalRepr (Tuple a b) where
    fromRepr = Repr.unwrap >>>
        case _ of
            Tup reprA reprB -> (/\) <$> fromRepr (Repr reprA) <*> fromRepr (Repr reprB)
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
            -- TODO: Tup default
            _ -> exprCtor "None"
    typeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
                  case typeStr of
                    "Int" -> typeCtor "Int"
                    "String" -> typeCtor "String"
                    "Unit" -> typeCtor "Unit"
                    -- TODO: Tup type
                    _ -> typeCtor "MinimalRepr"
    valueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
                  case NT.unwrap <$> mbType of
                     Just "Int" -> exprInt $ fromMaybe 0 $ Int.fromString valueStr
                     Just "String" -> exprString valueStr
                     Just "Unit" -> exprIdent "unit"
                     -- TODO: tuple value
                     _ -> if (valueStr == "unit")
                                then exprIdent "unit"
                                else exprCtor "None"