module Example.Toolkit.Minimal.StRepr where

import Prelude

import Partial.Unsafe (unsafePartial)

import Data.Newtype (unwrap, wrap) as NT
-- import Data.String.Read (read)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)

import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)

import Tidy.Codegen (exprIdent, exprInt, exprString, exprOp, typeCtor, typeOp, binaryOp)

import Noodle.Text.NdfFile.Types (EncodedValue(..))
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, pTypeFor, pDefaultFor, pValueFor, class ParseableRepr)

import Example.Toolkit.Minimal.PatchState (State) as Patch
import Example.Toolkit.Minimal.Node.ModifiesPatch as ModifiesPatch


data MinimalStRepr
    = NoSt
    | UnitSt
    | StrSt String
    | PState ((Int /\ String) /\ String)


derive instance Eq MinimalStRepr


instance HasFallback MinimalStRepr where
    fallback = NoSt


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
instance StRepr ModifiesPatch.State MinimalStRepr where
    to :: ModifiesPatch.State -> MinimalStRepr
    to (ModifiesPatch.State ({ intVal, strVal } /\ nodeVal)) = PState $ (intVal /\ strVal) /\ nodeVal
    from :: MinimalStRepr -> Maybe ModifiesPatch.State
    from = case _ of
        PState ((intVal /\ strVal) /\ nodeVal) -> Just $ ModifiesPatch.State $ { intVal, strVal } /\ nodeVal
        _ -> Nothing


instance CodegenRepr MinimalStRepr where
    reprModule = const "Example.Toolkit.Minimal.ChRepr"
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
            _ -> exprIdent "unit"
    pTypeFor = const $ unsafePartial $ \mbType ->
                  case NT.unwrap <$> mbType of
                    Just "StrSt" -> typeCtor "String"
                    Just "UnitSt" -> typeCtor "Unit"
                    Just "PState" ->
                        typeOp (typeCtor "Int")
                            [ binaryOp "/\\" $ typeCtor "String"
                            , binaryOp "/\\" $ typeCtor "String"
                            ]
                    _ -> typeCtor "Unit"
    pValueFor = const $ unsafePartial $ \mbType (EncodedValue valueStr) ->
                  case NT.unwrap <$> mbType of
                     Just "StrSt" -> exprString valueStr
                     Just "UnitSt" -> exprIdent "unit"
                     Just "PState" -> exprIdent "foo" -- FIXME:
                     _ -> exprIdent "unit"
    fTypeFor prepr = pTypeFor prepr
    fDefaultFor prepr = pDefaultFor prepr
    fValueFor prepr = pValueFor prepr


instance Show MinimalStRepr where
    show =
        case _ of
            NoSt -> "<None>"
            PState st -> "<PSt:" <> show st <> ">"
            StrSt str -> "<Str:" <> str <> ">"
            UnitSt -> "<Unit>"
