module Hydra.Repr.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype as NT
import Data.Tuple.Nested ((/\))

import Type.Proxy (Proxy(..))
import Partial.Unsafe (unsafePartial)

import PureScript.CST.Types (Type, Expr, Declaration) as CST
import Tidy.Codegen

import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (from, to) as StRepr
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, pDefaultFor)
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))

import Hydra.Types as HT


data StateRepr
    = Val HT.Value



pState = Proxy :: _ StateRepr


instance (NT.Newtype w Unit) => StRepr w StateRepr where
  to :: w -> StateRepr
  to = NT.unwrap >>> (const $ Val HT.None)
  from :: StateRepr -> Maybe w
  from = map NT.wrap <<< (const $ Just unit)



instance CodegenRepr StateRepr where
    reprModule = const "Hydra.Repr.State"
    reprTypeName = const "StateRepr"
    reprType =    const $ unsafePartial $ typeCtor "StateRepr"
    pTypeFor =    typeFor
    pDefaultFor = defaultFor
    pValueFor =   const $ unsafePartial $ \_ (EncodedValue valueStr) -> exprCtor "StateRepr"
    fTypeFor =    typeFor
    fDefaultFor = defaultFor
    fValueFor =   const $ unsafePartial $ \mbType (EncodedValue valueStr) -> exprCtor "StateRepr"


typeFor :: Proxy StateRepr -> Maybe EncodedType -> CST.Type Void
typeFor = const $ unsafePartial $ \mbEncType ->
    case NT.unwrap <$> mbEncType of
        Just "Unit" -> typeCtor "Unit"
        _ -> typeCtor "Unit"



defaultFor :: Proxy StateRepr -> Maybe EncodedType -> CST.Expr Void
defaultFor = const $ unsafePartial $ \mbType ->
    case mbType of
        Just (EncodedType encType) ->
            case encType of
                "Unit" -> exprIdent "unit"
                _ -> exprIdent "unit"
        Nothing -> exprIdent "unit"