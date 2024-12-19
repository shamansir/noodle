module Example.Toolkit.Minimal.Repr where

import Prelude

import Partial.Unsafe (unsafePartial)

import Data.Newtype (unwrap, wrap) as NT
-- import Data.String.Read (read)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple, uncurry, curry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ChRepr (class ToChRepr, class FromChRepr, toChRepr, fromChRepr, ChRepr(..))
import Noodle.Repr.ChRepr (fromEq, toEq, wrap, unwrap, ensureTo, ensureFrom) as Repr
import Noodle.Repr.StRepr (class StRepr)

import Tidy.Codegen (exprCtor, exprIdent, exprInt, exprString, typeCtor)

import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, pTypeFor, pDefaultFor, pValueFor)


import Example.Toolkit.Minimal.PatchState (State(..)) as Patch


data MinimalRepr
    = None
    | UnitV
    | Int Int
    | Str String
    | Tup MinimalRepr MinimalRepr -- FIXME: Store just Int & String for Patch instead, no overcomplication


derive instance Eq MinimalRepr


instance FromChRepr MinimalRepr MinimalRepr where fromChRepr = Repr.fromEq
instance ToChRepr   MinimalRepr MinimalRepr where toChRepr   = Repr.toEq


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


instance ToChRepr Int MinimalRepr where toChRepr = Just <<< Repr.wrap <<< Int
instance ToChRepr String MinimalRepr where toChRepr = Just <<< Repr.wrap <<< Str
instance ToChRepr Unit MinimalRepr where toChRepr = Just <<< Repr.wrap <<< const UnitV

instance ToChRepr Patch.State MinimalRepr where
    toChRepr = NT.unwrap >>> \{ intVal, strVal } -> toChRepr $ intVal /\ strVal

instance StRepr Unit MinimalRepr where
    to = const UnitV
    from = case _ of
        UnitV -> Just unit
        _ -> Nothing
instance StRepr String MinimalRepr where
    to = Str
    from = case _ of
        Str str -> Just str
        _ -> Nothing
instance StRepr (Tuple Patch.State String) MinimalRepr where
    to :: Patch.State /\ String -> MinimalRepr
    to = bimap (NT.unwrap >>> packP) Str >>> uncurry Tup
        where packP { intVal, strVal } = Tup (Int intVal) (Str strVal)
    from :: MinimalRepr -> Maybe (Patch.State /\ String)
    from = case _ of
        Tup (Tup (Int intVal) (Str strVal)) (Str nodeVal) -> Just $ (NT.wrap { intVal, strVal }) /\ nodeVal
        _ -> Nothing

instance
    ( ToChRepr a MinimalRepr
    , ToChRepr b MinimalRepr
    )
    => ToChRepr (Tuple a b) MinimalRepr where
    toChRepr = Just <<< Repr.wrap <<<
        \(a /\ b) ->
            Tup
                (Repr.unwrap $ Repr.ensureTo a)
                (Repr.unwrap $ Repr.ensureTo b)


instance FromChRepr MinimalRepr Int where
    fromChRepr = Repr.unwrap >>>
        case _ of
            Int n -> Just n
            _ -> Nothing
instance FromChRepr MinimalRepr String where
    fromChRepr = Repr.unwrap >>>
        case _ of
            Str str -> Just str
            _ -> Nothing
instance FromChRepr MinimalRepr Unit where
    fromChRepr = Repr.unwrap >>>
        case _ of
            UnitV -> Just unit
            _ -> Nothing
instance FromChRepr MinimalRepr Patch.State where
    fromChRepr = Repr.unwrap >>>
        case _ of
            Tup (Int intVal) (Str strVal) -> Just $ Patch.State { intVal, strVal }
            _ -> Nothing
instance (FromChRepr MinimalRepr a, FromChRepr MinimalRepr b) => FromChRepr MinimalRepr (Tuple a b) where
    fromChRepr = Repr.unwrap >>>
        case _ of
            Tup reprA reprB -> (/\) <$> fromChRepr (ChRepr reprA) <*> fromChRepr (ChRepr reprB)
            _ -> Nothing


instance CodegenRepr MinimalRepr where
    reprModule = const "Example.Toolkit.Minimal.Repr"
    reprTypeName = const "MinimalRepr"
    reprType = const $ unsafePartial $ typeCtor "MinimalRepr"
    pDefaultFor = const $ unsafePartial $ \mbType ->
        case NT.unwrap <$> mbType of
            Just "Int" -> exprInt 0
            Just "String" -> exprString ""
            Just "Unit" -> exprIdent "unit"
            -- TODO: Tup default
            _ -> exprCtor "None"
    pTypeFor = const $ unsafePartial $ \(EncodedType typeStr) ->
                  case typeStr of
                    "Int" -> typeCtor "Int"
                    "String" -> typeCtor "String"
                    "Unit" -> typeCtor "Unit"
                    -- TODO: Tup type
                    _ -> typeCtor "MinimalRepr"
    pValueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
                  case NT.unwrap <$> mbType of
                     Just "Int" -> exprInt $ fromMaybe 0 $ Int.fromString valueStr
                     Just "String" -> exprString valueStr
                     Just "Unit" -> exprIdent "unit"
                     -- TODO: tuple value
                     _ -> if (valueStr == "unit")
                                then exprIdent "unit"
                                else exprCtor "None"
    fTypeFor prepr = pTypeFor prepr
    fDefaultFor prepr = pDefaultFor prepr
    fValueFor prepr = pValueFor prepr