module StarterTk.Repr.StRepr where

import Prelude

import Data.Maybe (Maybe(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen
     (exprCtor, typeCtor)

import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr)
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))

import StarterTk.Library.Simple.Gennum as Gennum
import StarterTk.Library.Simple.Metro as Metro


data StateRepr
    = StateRepr
    | Gennum Gennum.State
    | Metro Metro.State


instance HasFallback StateRepr where
    fallback = StateRepr


instance StRepr Gennum.State StateRepr where
    from = case _ of
        Gennum gn -> Just gn
        _ -> Nothing
    to = Gennum
instance StRepr Metro.State StateRepr where
    from = case _ of
        Metro ms -> Just ms
        _ -> Nothing
    to = Metro
-- else instance StRepr StateRepr NoState where
--     from = const NoState
--     to = const StateRepr
instance StRepr Unit StateRepr where
    from = const $ Just unit
    to = const StateRepr


instance CodegenRepr StateRepr where
    reprModule = const "StarterTk.Repr.StRepr"
    reprTypeName = const "StateRepr" -- FIXME: "StateRepr"
    reprType =    const $ unsafePartial $ typeCtor "StateRepr" -- FIXME: "StateRepr"
    pTypeFor =    const $ unsafePartial $ \(EncodedType _) -> typeCtor "StateRepr"
    pDefaultFor = const $ unsafePartial $ \_ -> exprCtor "StateRepr"
    pValueFor =   const $ unsafePartial $ \_ (EncodedValue valueStr) -> exprCtor "StateRepr"
    fTypeFor =    const $ unsafePartial $ \(EncodedType _) -> typeCtor "StateRepr"
    fDefaultFor = const $ unsafePartial $ \mbType -> exprCtor "StateRepr"
    fValueFor =   const $ unsafePartial $ \mbType (EncodedValue valueStr) -> exprCtor "StateRepr"
