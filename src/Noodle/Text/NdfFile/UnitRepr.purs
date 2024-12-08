module Noodle.Text.NdfFile.UnitRepr where

import Prelude


import Data.Maybe (Maybe(..))

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen (exprCtor, typeCtor, declImport)

import Noodle.Repr (class HasFallback, wrap, unwrap, class ToRepr, class FromRepr)
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.NdfFile.FamilyDef.Codegen (class CodegenRepr, Options(..), groupPascalCase, familyPascalCase)


data UnitRepr = UnitRepr


instance CodegenRepr UnitRepr where
    reprModule = const "Noodle.Text.NdfFile.UnitRepr"
    reprTypeName = const "UnitRepr"
    reprType = const $ unsafePartial $ typeCtor "UnitRepr"
    typeFor = const $ unsafePartial $ const $ typeCtor "UnitRepr"
    defaultFor = const $ unsafePartial $ const $ exprCtor "UnitRepr"
    valueFor = const $ const $ const $ unsafePartial $ exprCtor "UnitRepr"


options :: Options UnitRepr
options = Options $
    { reprAt : { module_ : "Noodle.Text.NdfFile.UnitRepr", type_ : "UnitRepr" }
    , temperamentAlgorithm : Temperament.defaultAlgorithm
    , monadAt : { module_ : "Effect", type_ : "Effect" }
    , familyModuleName : \fgroup family -> "MyToolkit" <> "." <> groupPascalCase fgroup <> "." <> familyPascalCase family
    , prepr : (Proxy :: _ UnitRepr)
    , infoComment : Just $ \mbSource fgroup family ->
            "Generated by Noodle Codegen from NDF file. Group :: " <> show fgroup <> ". Family :: " <> show family <> "." <> case mbSource of
            Just src -> "\n\n[[ " <> src.line <> " ]] (#" <> show src.lineIndex <> ")"
            Nothing -> ""
    , imports : unsafePartial $
        [ declImport "Noodle.Text.NdfFile.UnitRepr" []
        ]
    }


instance HasFallback UnitRepr where
    fallback = UnitRepr


instance ToRepr UnitRepr UnitRepr
    where toRepr = Just <<< wrap


instance FromRepr UnitRepr UnitRepr
    where fromRepr = unwrap >>> Just