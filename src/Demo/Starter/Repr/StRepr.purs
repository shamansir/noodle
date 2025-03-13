module StarterTk.Repr.StRepr where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Newtype (unwrap) as NT

import Type.Proxy (Proxy)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Type, Expr, Declaration) as CST

import Tidy.Codegen

import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, pDefaultFor)
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
        Just "Gennum" ->
            typeRecord
                [ "signal" /\ typeApp (typeCtor "Maybe")
                    [ typeApp (typeCtor "Signal")
                        [ typeApp (typeCtor "Effect") [ typeCtor "Unit" ]
                        ]
                    ]
                ] Nothing
        Just "Metro" ->
            typeRecord
                [ "cancelPrev" /\ typeApp (typeCtor "Maybe")
                    [ typeApp (typeCtor "Effect") [ typeCtor "Unit" ]
                    ]
                ] Nothing
        _ -> typeCtor "Unit"



defaultFor :: Proxy StateRepr -> Maybe EncodedType -> CST.Expr Void
defaultFor = const $ unsafePartial $ \mbType ->
    case mbType of
        Just (EncodedType encType) ->
            case encType of
                "Unit" -> exprIdent "unit"
                "Gennum" ->
                    exprRecord
                        [ "signal" /\ exprCtor "Nothing"
                        ]
                "Metro" ->
                    exprRecord
                        [ "cancelPrev" /\ exprCtor "Nothing"
                        ]
                _ -> exprIdent "unit"
        Nothing -> exprIdent "unit"
