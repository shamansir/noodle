module Demo.Toolkit.Starter.Repr.Options where

import Prelude

import Data.Maybe (Maybe(..))

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Tidy.Codegen (declImport, declImportAs, importOp, importTypeOp, importValue)

import Noodle.Id (family) as Id
import Noodle.Fn.Shape.Temperament (defaultAlgorithm) as Temperament
import Noodle.Text.NdfFile.FamilyDef.Codegen (Options(..), familyPascalCase, groupPascalCase)

import Demo.Toolkit.Starter.Repr.StRepr (StateRepr)
import Demo.Toolkit.Starter.Repr.ChRepr (ValueRepr)


options :: Options StateRepr ValueRepr
options = Options $
    { streprAt : { module_ : "Demo.Toolkit.Starter.Repr", type_ : "StateRepr" }
    , chreprAt : { module_ : "Demo.Toolkit.Starter.Repr", type_ : "ValueRepr" }
    , temperamentAlgorithm : Temperament.defaultAlgorithm
    , monadAt : { module_ : "Effect", type_ : "Effect" }
    , familyModuleName : \fgroup family -> "StarterTk" <> "." <> groupPascalCase fgroup <> "." <> familyPascalCase family
    , pstrepr : (Proxy :: _ StateRepr)
    , pchrepr : (Proxy :: _ ValueRepr)
    , infoComment : Just $ \mbSource fgroup family ->
            "Generated by Noodle Codegen from NDF file. Group :: " <> show fgroup <> ". Family :: " <> show family <> "." <> case mbSource of
            Just src -> "\n\n[[ " <> src.line <> " ]] (#" <> show src.lineIndex <> ")"
            Nothing -> ""
    , tkImports : genericImports
    , familyImports : \familyR ->
        genericImports <> case Id.family familyR of
            "gennum" ->
                unsafePartial $
                    [ declImport "Effect.Class" [ importValue "liftEffect" ]
                    , declImport "Effect.Random" [ importValue "random" ]
                    , declImport "Signal" [ importOp "~>" ]
                    , declImportAs "Signal.Extra" [ ] "SignalX"
                    , declImportAs "Signal.Time" [ importValue "every" ] "Signal"
                    ]
            _ -> []
    }
    where
        genericImports = unsafePartial $
            [ declImport "Data.Tuple.Nested" [ importOp "/\\", importTypeOp "/\\" ]
            , declImportAs "Demo.Toolkit.Starter.Repr" [] "VR"
            ]