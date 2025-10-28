module Noodle.Text.NdfFile.Codegen where

import Prelude
import Tidy.Codegen

import Color as Color

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import PureScript.CST.Types (ImportDecl, Module)
import PureScript.CST.Types (Type, Expr, Declaration) as CST
import Tidy.Codegen.Class (class OverLeadingComments)

import Data.Array ((:))
import Data.Array (uncons, reverse, singleton, mapWithIndex, index, nub) as Array
import Data.String (replace, Pattern(..), Replacement(..)) as String
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.String (toUpper) as String
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)

import Noodle.Fn.Signature (Argument, Output, extract, argName, argValue, outName, outValue, args, outs) as Sig
import Noodle.Fn.Signature (Signature, SignatureS, SignatureX, toSignature)
import Noodle.Id (FamilyR, GroupR)
import Noodle.Id (toolkit, group, family) as Id
import Noodle.Text.NdfFile.FamilyDef (FamilyDef(..))
import Noodle.Text.NdfFile.FamilyDef (group, family, processCode) as FamilyDef
import Noodle.Text.NdfFile.FamilyDef.Codegen as FCG
import Noodle.Text.NdfFile.Types (Source, ChannelDef, EncodedType, EncodedValue)
import Noodle.Text.NdfFile.Types (encodedTypeOf, encodedValueOf) as Ndf
import Noodle.Text.NdfFile.FamilyDef.ProcessCode as PC
import Noodle.Toolkit (Name) as Toolkit
import Noodle.Ui.Palette.Item (Item, colorOf) as Palette
import Noodle.Ui.Palette.AutoColor (group) as AutoColor
-- import Tidy.Codegen.Monad (codegenModule, importFrom, importOpen, importType, importTypeAll, importTypeOp, importValue)


newtype FilePath = FilePath String -- TODO: replace with Node.FS FilePath
newtype FileContent = FileContent String
newtype GenRootPath = GenRootPath String
newtype ModulePrefix = ModulePrefix String

derive newtype instance Show FilePath
derive instance Newtype GenRootPath _
derive instance Newtype ModulePrefix _


derive newtype instance Eq FilePath
derive newtype instance Ord FilePath


codegen
    :: forall strepr chrepr
    .  FCG.CodegenRepr strepr
    => FCG.CodegenRepr chrepr
    => Toolkit.Name
    -> FCG.Options strepr chrepr
    -> Array (Maybe Source /\ FamilyDef)
    -> Map FilePath FileContent
codegen tkName options definitions =
    definitions
    # foldr genModule Map.empty
    # Map.insert
        (FilePath $ toolkitFile genRoot tkName)
        (generateToolkit tkName options $ Tuple.snd <$> definitions)
    where
        genRoot = GenRootPath ""
        filePathFor = FilePath <<< moduleFile genRoot tkName
        genModule (mbSource /\ FamilyDef familyDef) =
            -- toCode (ToCode.pureScript) genOptions familyDef
            Map.insert (filePathFor $ FamilyDef familyDef) $ FileContent $ FCG.generate options mbSource familyDef


codegenRaw
    :: forall strepr chrepr
    .  Toolkit.Name
    -> FCG.Options strepr chrepr
    -> Array (Maybe Source /\ FamilyDef)
    -> FilePath /\ FileContent
codegenRaw tkName options definitions =
    FilePath (rawToolkitFile genRoot tkName) /\ (FileContent $ injectFamilyProcessFunctions <<< printModule <<< generateRawToolkitModule tkName options $ Tuple.snd <$> definitions)
    where
        genRoot = GenRootPath ""
        injectFamilyProcessFunctions moduleString =
            foldl
                (\mstr familyDef ->
                    injectProcessFor (FamilyDef.family familyDef) (FamilyDef.processCode familyDef) mstr
                )
                moduleString
                $ Tuple.snd <$> definitions
        injectProcessFor familyR processCode =
            String.replace
                (String.Pattern $ FCG.__raw_process_pattern familyR)
                (String.Replacement $ PC.process (PC.Indent $ FCG.__raw_process_indent <> "    ") $ updateProcessCode processCode)
        updateProcessCode = case _ of
            PC.Encoded _ processCode -> PC.Encoded PC.Raw processCode -- FIXME:
            other -> other



generateToolkit :: forall strepr chrepr. FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> Array FamilyDef -> FileContent
generateToolkit tkName options = FileContent <<< printModule <<< generateToolkitModule tkName options


generateToolkitModule :: forall strepr chrepr. FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> Array FamilyDef -> Module Void
generateToolkitModule tkName (FCG.Options opts) definitionsArray
    = unsafePartial $ module_ (opts.toolkitModuleName tkName)
        [ ]
        (
            [ declImport "Prelude" [ ]
            , declImport "Effect" [ importType "Effect" ]
            , declImport "Effect.Class" [ importClass "MonadEffect" ]
            , declImportAs "Color" [] "Color"
            , declImport "Data.Maybe" [ importTypeAll "Maybe" ]
            , declImport "Data.Tuple.Nested" [ importOp "/\\" ]
            , declImport "Type.Data.List" [ importTypeOp ":>" ]
            , declImport "Type.Data.List.Extra" [ importType "TNil", importClass "Put" ]
            , declImport "Type.Proxy" [ importTypeAll "Proxy" ]
            , declImportAs "Noodle.Id" [ importValue "toolkitR", importValue "family", importType "FamilyR", importValue "unsafeGroupR", importValue "group", importType "NodeR" ] "Id"
            , declImport "Noodle.Fn.Signature" [ importValue "sig", importClass "PossiblyToSignature" ]
            , declImportAs "Noodle.Fn.Signature" [ importValue "in_", importValue "inx_", importValue "out_", importValue "outx_", importValue "toChanneled" ] "Sig"
            , declImport "Noodle.Toolkit" [ importType "Toolkit", importType "ToolkitKey", importClass "MarkToolkit", importClass "IsToolkit", importClass "HasChRepr", importClass "InitPatchState", importClass "FromToPatchState", importValue "markGroup" ]
            , declImportAs "Noodle.Toolkit" [ importValue "empty", importValue "register" ] "Toolkit"
            , declImport "Noodle.Toolkit.Families" [ importType "Families", importType "F", importClass "RegisteredFamily" ]
            , declImport "Noodle.Repr.HasFallback" [ importValue "fallback" ]
            , declImport "Noodle.Repr.ValueInChannel" [ importType "ValueInChannel" ]
            , declImport "Cli.Class.CliRenderer" [ importClass "CliRenderer", importClass "CliRawRenderer", importClass "CliEditor" ]
            , declImport "Web.Class.WebRenderer" [ importClass "WebRenderer", importClass "WebRawRenderer", importClass "WebEditor" ]
            , declImportAs "Web.Class.WebRenderer" [ importType "InletPath" ] "WR"
            , declImport "Web.Components.ValueEditor" [ importType "ValueEditor" ]
            , declImportAs "Web.Components.ValueEditor" [ importType "EditorId" ] "ValueEditor"
            , declImportAs "Web.Components.ValueEditor.Numeric" [ ] "NumericVE"
            , declImportAs "Halogen" [ importType "Component", importType "RefLabel" ] "H"
            ]
            <> (defToModuleImport <$> definitions) <>
            [ declImport opts.streprAt.module_ [ importType opts.streprAt.type_ ]
            , declImport opts.chreprAt.module_ [ importType opts.chreprAt.type_ ]
            ]
            <> opts.tkImports
        )
        [ declTypeSignature familiesCtor $ typeCtor "Families"
        , declType familiesCtor [] familiesTList
        , declForeignData toolkitKey $ typeCtor "ToolkitKey"
        , declSignature "toolkit"
            $ typeApp (typeCtor "Toolkit")
                [ typeCtor toolkitKey
                , typeCtor familiesCtor
                , typeCtor opts.streprAt.type_
                , typeCtor opts.chreprAt.type_
                , typeCtor $ opts.monadAt.type_
                ]
        , declValue "toolkit" [] registerFamilies
        , declInstance Nothing [] "HasChRepr" [ typeCtor toolkitKey, typeCtor opts.chreprAt.type_ ] []
        , declInstance Nothing [] "IsToolkit" [ typeCtor toolkitKey ]
            [ instValue "name" [ binderWildcard ]
                $ exprString $ Id.toolkit tkName
            , instValue "groupOf" [ binderWildcard ]
                $ exprOp (exprIdent "Id.family")
                    [ binaryOp ">>>" $ exprParens $ exprCase [ exprSection ]
                        $ (groupOfFamilyBranch <$> definitionsArray)
                        <> [ caseBranch [ binderWildcard ] $ exprString "unknown" ]
                    , binaryOp ">>>" $ exprIdent "Id.unsafeGroupR"
                    ]
            ]
        , declInstance Nothing [ monadEffectReq ] "CliRenderer" [ typeCtor toolkitKey, typeCtor familiesCtor, typeCtor opts.chreprAt.type_, typeVar "m" ]
            [ instValue "cliSize" _5binders $ exprCtor "Nothing"
            , instValue "renderCli" _5binders $ exprCtor "Nothing" -- exprApp (exprCtor "Just") [ exprApp (exprIdent "pure") [ exprIdent "unit" ] ]
            ]
        , declInstance Nothing [ monadEffectReq ] "CliRawRenderer" [ typeCtor toolkitKey, typeCtor familiesCtor, typeCtor opts.chreprAt.type_, typeVar "m" ]
            [ instValue "cliSizeRaw" _5binders $ exprCtor "Nothing"
            , instValue "renderCliRaw" _5binders $ exprCtor "Nothing" -- exprApp (exprCtor "Just") [ exprApp (exprIdent "pure") [ exprIdent "unit" ] ]
            ]
        , declInstance Nothing [ ] "CliEditor" [ typeCtor toolkitKey, typeCtor opts.chreprAt.type_ ]
            [ instValue "cliEditorFor" (binderWildcard : _5binders) $ exprCtor "Nothing"
            ]
        , declInstance Nothing [ monadEffectReq ] "WebEditor" [ typeCtor toolkitKey, typeCtor opts.chreprAt.type_, typeVar "m" ]
            [ instValue "spawnWebEditor" _4binders $ exprCtor "Nothing" {-
                $ exprWhere
                    (exprOp (exprCtor "Just")
                        [ binaryOp "$" $ exprApp (exprIdent "NumericVE.editor") [ exprIdent "toNumber", exprIdent "fromNumber" ]
                        ]
                    )
                [ letBinder (binderVar "toNumber")
                    $ exprCase [ exprSection ] $ -- FIXME: generate it from options
                        [ caseBranch [ binderCtor "HYDRAW.Value" [ binderCtor "HYDRA.Number" [ binderVar "n" ] ] ] $ exprApp (exprCtor "Just") [ exprIdent "n" ]
                        , caseBranch [ binderWildcard ] $ exprCtor "Nothing"
                        ]
                , letBinder (binderVar "fromNumber")
                    $ exprOp (exprCtor "HYDRAW.Value")
                        [ binaryOp "<<<" $ exprApp (exprCtor "HYDRA.Number") [] ]
                ] -}
            ]
        , declInstance Nothing [] "MarkToolkit" [ typeCtor toolkitKey ]
            [ instValue "markGroup" [ binderWildcard ]
                $ exprOp (exprIdent "Id.group")
                    [ binaryOp ">>>" $ exprParens $ exprCase [ exprSection ]
                        $ (Array.mapWithIndex groupColorBranch groupArray)
                        <> [ caseBranch [ binderWildcard ] $ rgbColorExpr 255 255 255 ]
                    ]
            , instValue "markFamily" [ binderVar "ptk" ]
                $ exprOp (exprIdent "const")
                [ binaryOp "<<<" $ exprApp (exprIdent "markGroup") [ exprIdent "ptk" ] ]
            ]
        , declInstance Nothing [ monadEffectReq ] "InitPatchState" [ typeCtor toolkitKey, typeCtor opts.pstreprType, typeVar "m" ]
            [ instValue "initPatch" [] $ exprApp (exprIdent "const") [ exprApp (exprIdent "Patch.init") [] ] ]
        , declInstance Nothing [ ] "FromToPatchState" [ typeCtor toolkitKey, typeCtor opts.pstreprType, typeCtor opts.streprAt.type_ ]
            [ instValue "loadFromPatch" _4binders $ exprCtor "Nothing"
            , instValue "putInPatch" _3binders $ exprIdent "identity"
            ]
        , generatePossiblyToSignatureInstance tkName (FCG.Options opts) definitionsArray
        ]
    where
        monadEffectReq = unsafePartial $ typeApp (typeCtor "MonadEffect") [ typeVar "m" ]
        toolkitKey = String.toUpper $ Id.toolkit tkName -- Id.toolkit tkName <> "Key"
        familiesCtor = Id.toolkit tkName <> "Families"
        groupAndFamily :: FamilyDef -> GroupR /\ FamilyR
        groupAndFamily fdef = FamilyDef.group fdef /\ FamilyDef.family fdef
        definitions :: Array (GroupR /\ FamilyR)
        definitions = groupAndFamily <$> definitionsArray
        fModuleName :: GroupR /\ FamilyR -> String
        fModuleName (group /\ family) = FCG.groupPascalCase group <> "." <> FCG.familyPascalCase family
        referFamily :: GroupR /\ FamilyR -> String
        referFamily (group /\ family) = fModuleName (group /\ family) <> "." <> "family"
        referFamilyF :: GroupR /\ FamilyR -> String
        referFamilyF (group /\ family) = fModuleName (group /\ family) <> "." <> "F"
        defToModuleImport :: Partial => GroupR /\ FamilyR -> ImportDecl Void
        defToModuleImport (group /\ family) =
            declImportAs
                (opts.familyModuleName group family)
                []
                $ fModuleName (group /\ family)
        familiesTList :: Partial => CST.Type Void
        familiesTList =
            case Array.uncons definitions of
                Just { head, tail } ->
                    typeOp (typeCtor $ referFamilyF head)
                        $ (binaryOp ":>" <$> typeCtor <$> referFamilyF <$> tail)
                            <> [ binaryOp ":>" $ typeCtor "TNil"]
                Nothing -> typeCtor "TNil"
        registerFamilies :: Partial => CST.Expr Void
        registerFamilies =
            case Array.uncons definitions of
                Just { head, tail } ->
                    exprOp (exprApp (exprIdent "Toolkit.register") [ exprIdent $ referFamily head ])
                        $ (binaryOp "$"
                            <$> exprApp (exprIdent "Toolkit.register")
                            <$> Array.singleton
                            <$> exprIdent
                            <$> referFamily
                            <$> tail)
                            <> [ binaryOp "$"
                                $ exprApp (exprIdent "Toolkit.empty")
                                    [ exprTyped
                                        ( exprCtor "Proxy" )
                                        $ typeApp typeWildcard [ typeCtor toolkitKey ]
                                    , exprApp (exprIdent "Id.toolkitR")
                                        [ exprString $ Id.toolkit tkName ]
                                    ]
                                ]
        groupArray :: Array GroupR
        groupArray = Array.nub $ FamilyDef.group <$> definitionsArray
        _3binders = [ binderWildcard, binderWildcard, binderWildcard ]
        _4binders = binderWildcard : _3binders
        _5binders = binderWildcard : _4binders
        rgbColorExpr :: Partial => Int -> Int -> Int -> CST.Expr Void
        rgbColorExpr r g b = exprApp (exprIdent "Color.rgb") [ exprInt r, exprInt g, exprInt b ]
        groupOfFamilyBranch :: Partial => FamilyDef -> _
        groupOfFamilyBranch fdef =
            caseBranch
                [ binderString $ Id.family $ FamilyDef.family fdef ]
                $ exprString $ Id.group $ FamilyDef.group fdef
        groupColorBranch :: Partial => Int -> GroupR -> _
        groupColorBranch idx groupR =
            caseBranch
                [ binderString $ Id.group groupR ]
                $ case Array.index AutoColor.group idx of
                    Just paletteItem ->
                        case Color.toRGBA $ Palette.colorOf paletteItem of
                            { r, g, b, a } -> rgbColorExpr r g b
                    Nothing -> rgbColorExpr 255 255 255


generateRawToolkitModule :: forall strepr chrepr. Toolkit.Name -> FCG.Options strepr chrepr -> Array FamilyDef -> Module Void
generateRawToolkitModule tkName (FCG.Options opts) definitionsArray
    = unsafePartial $ module_ (opts.toolkitModuleName tkName <> "Raw")
        []
        (
            [ declImport "Prelude" [ ]
            , declImport "Effect.Class" [ importClass "MonadEffect" ]
            , declImportAs "Effect.Exception" [ ] "Ex"
            , declImport "Control.Monad.Error.Class" [ importClass "MonadThrow" ]
            , declImport "Data.Maybe" [ importTypeAll "Maybe" ]
            , declImport "Type.Proxy" [ importTypeAll "Proxy" ]
            , declImport "Type.Data.List.Extra" [ importType "TNil" ]
            , declImportAs "Noodle.Id" [ importValue "toolkitR" ] "Id"
            , declImport "Noodle.Toolkit" [ importType "Toolkit", importType "ToolkitKey" ]
            , declImportAs "Noodle.Toolkit" [ importValue "empty" ] "Toolkit"
            , declImportAs "Noodle.Unsafe.QuickMake.RawToolkit" [ importValue "qregister" ] "Toolkit"
            , declImportAs "Noodle.Unsafe.RawProcess" [ ] "RP"
            , declImportAs "Noodle.Repr.ValueInChannel" [] "ViC"
            , declImport opts.streprAt.module_ [ importType opts.streprAt.type_ ]
            , declImport opts.chreprAt.module_ [ importType opts.chreprAt.type_ ]
            ]
            <> opts.tkImports
        )
        [ declForeignData toolkitKey $ typeCtor "ToolkitKey"
        , declSignature "toolkit"
            $ typeForall [ typeVar "m" ]
            $ typeConstrained [ monadEffectReq, monadThrowReq ]
            $ typeApp (typeCtor "Toolkit")
                [ typeCtor toolkitKey
                , typeCtor "TNil"
                , typeCtor opts.streprAt.type_
                , typeCtor opts.chreprAt.type_
                , typeVar "m"
                ]
        , declValue "toolkit" [] registerFamiliesRaw
        ]
        where
            monadEffectReq = unsafePartial $ typeApp (typeCtor "MonadEffect") [ typeVar "m" ]
            monadThrowReq = unsafePartial $ typeApp (typeCtor "MonadThrow") [ typeCtor "Ex.Error",  typeVar "m" ]
            toolkitKey = String.toUpper $ Id.toolkit tkName
            channelExpr :: Partial => String /\ ChannelDef -> CST.Expr Void
            channelExpr (name /\ chdef) =
                exprRecord
                    [ "name" /\ exprString name
                    , "tag" /\ (exprString $ maybe "-" unwrap $ Ndf.encodedTypeOf chdef) -- FIXME: use default tag instead of "-"
                    , "value" /\ (maybe (exprCtor "Nothing") (exprApp (exprCtor "Just") <<< Array.singleton <<< exprString <<< unwrap) $ Ndf.encodedValueOf chdef)
                    ]
            registerFamilyRaw :: Partial => FamilyDef -> CST.Expr Void
            registerFamilyRaw familyDef =
                let
                    familySig = (toSignature (Proxy :: _ Void) familyDef) :: Signature ChannelDef ChannelDef
                    inletsDefs = Sig.args familySig
                    outletsDefs = Sig.outs familySig
                    familyR = FamilyDef.family familyDef
                    familyIdStr = Id.family familyR
                in exprOp
                    (leading (blockComment familyIdStr <> lineBreaks 1)
                        $ exprApp (exprIdent "Toolkit.qregister")
                        [ exprString familyIdStr
                        , exprArray $ channelExpr <$> inletsDefs
                        , exprArray $ channelExpr <$> outletsDefs ]
                    )
                    [ binaryOp "$" $ FCG.__raw_process_expr familyR ]
            registerFamiliesRaw :: Partial => CST.Expr Void
            registerFamiliesRaw =
                exprOp
                    (exprApp (exprIdent "Toolkit.empty")
                                [ exprTyped
                                    ( exprCtor "Proxy" )
                                    $ typeApp typeWildcard [ typeCtor toolkitKey ]
                                , exprApp (exprIdent "Id.toolkitR")
                                    [ exprString $ Id.toolkit tkName ]
                                ]
                    )
                    $ binaryOp "#" <$> registerFamilyRaw <$> definitionsArray


generatePossiblyToSignatureInstance :: forall strepr chrepr. Partial => FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> Array FamilyDef -> CST.Declaration Void
generatePossiblyToSignatureInstance tkName (FCG.Options opts) definitionsArray =
    declInstance Nothing [] "PossiblyToSignature"
        [ typeCtor toolkitKey
        , typeApp (typeCtor "ValueInChannel") [ typeCtor opts.chreprAt.type_ ]
        , typeApp (typeCtor "ValueInChannel") [ typeCtor opts.chreprAt.type_ ]
        , typeCtor "Id.FamilyR"
        ]
        [ instValue "possiblyToSignature" [ binderWildcard ]
            $ exprOp (exprIdent "Id.family")
            [ binaryOp ">>>"
                $ exprCase [ exprSection ]
                    $ (fnDefBranch <$> definitionsArray)
                    <> [ caseBranch [ binderWildcard ] $ exprCtor "Nothing" ]
            , binaryOp ">>>"
                $ exprApp (exprIdent "map") [ exprIdent "Sig.toChanneled" ]
            ]
        ]
    where
        toolkitKey = String.toUpper $ Id.toolkit tkName -- Id.toolkit tkName <> "Key"
        fnDefBranch :: Partial => FamilyDef -> _
        fnDefBranch fdef =
            case (unwrap $ toSignature (Proxy :: _ Void) fdef :: SignatureS ChannelDef ChannelDef) of
                name /\ inlets /\ outlets ->
                    caseBranch
                        [ binderString name ]
                        $ exprOp ( exprCtor "Just" )
                            [ binaryOp "$"
                                $ exprApp (exprIdent "sig")
                                    [ exprString name
                                    , exprArray $ inletExpr  <$> inlets
                                    , exprArray $ outletExpr <$> outlets
                                    ]
                            ]
        inletExpr :: Partial => Sig.Argument ChannelDef -> CST.Expr Void
        inletExpr chdef = case chdef # Sig.argValue # unwrap of
            { mbType, mbDefault } -> case mbDefault of
                Just _ ->
                    exprOp
                        (exprApp (exprIdent "Sig.in_") [ exprString $ Sig.argName chdef ])
                        [ binaryOp "$" $ qChFullValue mbType mbDefault ]
                Nothing ->
                    exprApp (exprIdent "Sig.inx_") [ exprString $ Sig.argName chdef ]
        outletExpr :: Partial => Sig.Output ChannelDef -> CST.Expr Void
        outletExpr chdef = case chdef # Sig.outValue # unwrap of
            { mbType, mbDefault } -> case mbDefault of
                Just _ ->
                    exprOp
                        (exprApp (exprIdent "Sig.out_") [ exprString $ Sig.outName chdef ])
                        [ binaryOp "$" $ qChFullValue mbType mbDefault ]
                Nothing ->
                    exprApp (exprIdent "Sig.outx_") [ exprString $ Sig.outName chdef ]
        qChFullValue :: Maybe EncodedType -> Maybe EncodedValue -> CST.Expr Void
        qChFullValue mbDataType = maybe (FCG.fDefaultFor opts.pchrepr mbDataType) (FCG.fValueFor opts.pchrepr mbDataType)


moduleName :: Toolkit.Name -> GroupR -> FamilyR -> String
moduleName tkName group family =
  Id.toolkit tkName <> "." <> FCG.groupPascalCase group <> "." <> FCG.familyPascalCase family


moduleName' :: ModulePrefix -> Toolkit.Name -> GroupR -> FamilyR -> String
moduleName' modPrefix tkName group family =
  unwrap modPrefix <> "." <> moduleName tkName group family


modulePath :: GenRootPath -> Toolkit.Name -> FamilyDef -> String
modulePath genRoot tkName familyDef =
  unwrap genRoot <> "/" <> Id.toolkit tkName <> "/" <> (FCG.groupPascalCase $ FamilyDef.group familyDef)


moduleFile :: GenRootPath -> Toolkit.Name -> FamilyDef -> String
moduleFile genRoot tkName familyDef =
  modulePath genRoot tkName familyDef <> "/" <> (FCG.familyPascalCase $ FamilyDef.family familyDef) <> ".purs"


toolkitModuleName :: Toolkit.Name -> String
toolkitModuleName tkName =
    Id.toolkit tkName <> "." <> "Gen" <> "." <> "Toolkit"


toolkitModuleName' :: ModulePrefix -> Toolkit.Name -> String
toolkitModuleName' modPrefix tkName =
    unwrap modPrefix <> "." <> toolkitModuleName tkName


toolkitPath :: GenRootPath -> Toolkit.Name -> String
toolkitPath genRoot tkName = unwrap genRoot <> "/" <> Id.toolkit tkName


toolkitFile :: GenRootPath -> Toolkit.Name -> String
toolkitFile genRoot tkName =
    toolkitPath genRoot tkName <> "/" <> "Toolkit" <> ".purs"


rawToolkitFile :: GenRootPath -> Toolkit.Name -> String
rawToolkitFile genRoot tkName =
    toolkitPath genRoot tkName <> "/" <> "ToolkitRaw" <> ".purs"