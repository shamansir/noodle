module Demo.Toolkit.Starter.Repr.StRepr where

import Prelude

import Data.Maybe (Maybe(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen
     (exprIdent, typeCtor)

import Noodle.Repr.StRepr (class StRepr)
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr)
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..))

import StarterTk.Simple.Gennum as Gennum


data StateRepr
    = StateRepr
    | Gennum Gennum.State


instance StRepr Gennum.State StateRepr where
    from = case _ of
        StateRepr -> Nothing
        Gennum gn -> Just gn
    to = Gennum
-- else instance StRepr StateRepr NoState where
--     from = const NoState
--     to = const StateRepr
instance StRepr Unit StateRepr where
    from = const $ Just unit
    to = const StateRepr


instance CodegenRepr StateRepr where
    reprModule = const "Demo.Toolkit.Starter.Repr"
    reprTypeName = const "Unit" -- FIXME: "StateRepr"
    reprType =    const $ unsafePartial $ typeCtor "Unit" -- FIXME: "StateRepr"
    pTypeFor =    const $ unsafePartial $ \(EncodedType _) -> typeCtor "Unit"
    pDefaultFor = const $ unsafePartial $ \_ -> exprIdent "unit"
    pValueFor =   const $ unsafePartial $ \_ (EncodedValue valueStr) -> exprIdent "unit"
    fTypeFor =    const $ unsafePartial $ \(EncodedType _) -> typeCtor "Unit"
    fDefaultFor = const $ unsafePartial $ \mbType -> exprIdent "unit"
    fValueFor =   const $ unsafePartial $ \mbType (EncodedValue valueStr) -> exprIdent "unit"
