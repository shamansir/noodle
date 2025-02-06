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

import Tidy.Codegen (exprCtor, exprIdent, exprInt, exprString, exprOp, typeCtor, typeOp, binaryOp)

import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, pTypeFor, pDefaultFor, pValueFor)


import Example.Toolkit.Minimal.PatchState (State(..)) as Patch


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


instance FromChRepr MinimalVRepr MinimalVRepr where fromChRepr = Repr.fromEq
instance ToChRepr   MinimalVRepr MinimalVRepr where toChRepr   = Repr.toEq


instance Show MinimalVRepr where
    show =
        case _ of
            None -> "<None>"
            Int n -> show n
            Str str -> str
            UnitV -> "<Unit>"


instance HasFallback MinimalVRepr where
    fallback = None


instance ToChRepr Int MinimalVRepr where toChRepr = Just <<< Repr.wrap <<< Int
instance ToChRepr String MinimalVRepr where toChRepr = Just <<< Repr.wrap <<< Str
instance ToChRepr Unit MinimalVRepr where toChRepr = Just <<< Repr.wrap <<< const UnitV


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


instance FromChRepr MinimalVRepr Int where
    fromChRepr = Repr.unwrap >>>
        case _ of
            Int n -> Just n
            _ -> Nothing
instance FromChRepr MinimalVRepr String where
    fromChRepr = Repr.unwrap >>>
        case _ of
            Str str -> Just str
            _ -> Nothing
instance FromChRepr MinimalVRepr Unit where
    fromChRepr = Repr.unwrap >>>
        case _ of
            UnitV -> Just unit
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
                     Just "Int" -> exprInt $ fromMaybe 0 $ Int.fromString valueStr
                     Just "String" -> exprString valueStr
                     Just "Unit" -> exprIdent "unit"
                     _ -> if (valueStr == "unit")
                                then exprIdent "unit"
                                else exprCtor "None"
    fTypeFor prepr = pTypeFor prepr
    fDefaultFor prepr = pDefaultFor prepr
    fValueFor prepr = pValueFor prepr