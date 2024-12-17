module Demo.Toolkit.Starter.Repr.StRepr where

import Prelude

import Color as Color

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap) as NT
import Data.String (splitAt, drop) as String
import Data.Number (fromString) as Number
import Data.Array (length) as Array
import Data.Int (fromString, toNumber) as Int
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format as T

import Noodle.Fn.ToFn (class PossiblyToFn)

import Noodle.Ui.Cli.Palette.Item (colorOf) as C
import Noodle.Ui.Cli.Palette.Set.X11 as X11
import Noodle.Ui.Cli.Tagging.At (class At, at, ChannelLabel)
import Noodle.Ui.Cli.Palette.Mark (class Mark, mark)


import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen
    ( exprCtor, exprApp, exprIdent, exprBool, exprChar, exprNumber, exprRecord, exprInt, exprArray, exprOp, exprParens
    , binaryOp
    , typeCtor, typeApp, typeOp
    , declImport, declImportAs, importOp, importType, importTypeOp, importValue
    )

import Noodle.Id (family) as Id
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.HasFallback (fallback) as HF
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.ChRepr (class ToChRepr, class FromChRepr, fromChRepr)
import Noodle.Repr.ChRepr (wrap, unwrap, fromEq, toEq) as CR
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.NdfFile.FamilyDef.Codegen
    ( class CodegenRepr, Options(..)
    , class ValueCodegen
    , mkExpression
    , groupPascalCase, familyPascalCase
    , pDefaultFor, pValueFor
    )
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
