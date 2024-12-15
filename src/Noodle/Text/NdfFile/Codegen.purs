module Noodle.Text.NdfFile.Codegen where

import Prelude

import Type.Proxy (Proxy(..))

import Partial.Unsafe (unsafePartial)

import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (foldr)
import Data.Newtype (unwrap, wrap, class Newtype)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (uncons, reverse, singleton) as Array
import Data.String (toUpper) as String

import Noodle.Id (FamilyR, GroupR)
import Noodle.Toolkit (Name) as Toolkit
import Noodle.Id (toolkit, group, family) as Id
import Noodle.Text.NdfFile.Types (Source, ChannelDef, EncodedType, EncodedValue)
import Noodle.Text.NdfFile.FamilyDef (FamilyDef(..))
import Noodle.Text.NdfFile.FamilyDef (group, family) as FamilyDef
import Noodle.Text.NdfFile.FamilyDef.Codegen as FCG
import Noodle.Fn.ToFn (Fn, FnS, FnX, toFn)
import Noodle.Fn.ToFn (Argument, Output, extract, argName, argValue, outName, outValue) as Fn

import PureScript.CST.Types (ImportDecl, Module)
import PureScript.CST.Types (Type, Expr, Declaration) as CST

import Tidy.Codegen -- hiding (importType, importTypeOp, importValue, importTypeAll)
-- import Tidy.Codegen.Monad (codegenModule, importFrom, importOpen, importType, importTypeAll, importTypeOp, importValue)



newtype FilePath = FilePath String -- TODO: replace with Node.FS FilePath
newtype FileContent = FileContent String
newtype GenRootPath = GenRootPath String
newtype ModulePrefix = ModulePrefix String
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


generateToolkit :: forall strepr chrepr. FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> Array FamilyDef -> FileContent
generateToolkit tkName options = FileContent <<< printModule <<< generateToolkitModule tkName options


generateToolkitModule :: forall strepr chrepr. FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> Array FamilyDef -> Module Void
generateToolkitModule tkName (FCG.Options opts) definitionsArray
    = unsafePartial $ module_ (toolkitModuleName tkName)
        [ ]
        (
            [ declImport "Prelude" [ ] -- import Prelude (($), (#), (>>>), (<<<), pure, unit, const)
            , declImport "Effect" [ importType "Effect" ]
            , declImportAs "Color" [] "Color"
            , declImport "Data.Maybe" [ importTypeAll "Maybe" ]
            , declImport "Type.Data.List" [ importTypeOp ":>" ]
            , declImport "Type.Data.List.Extra" [ importType "TNil", importClass "Put" ]
            , declImport "Type.Proxy" [ importTypeAll "Proxy" ]
            , declImportAs "Noodle.Id" [ importValue "toolkitR", importValue "family", importType "FamilyR", importValue "unsafeGroupR" ] "Id"
            , declImport "Noodle.Fn.ToFn" [ importValue "fn", importClass "PossiblyToFn" ]
            , declImportAs "Noodle.Fn.ToFn" [ importValue "in_", importValue "inx_", importValue "out_", importValue "outx_" ] "Fn"
            , declImport "Noodle.Toolkit" [ importType "Toolkit", importType "ToolkitKey", importClass "MarkToolkit", importClass "IsToolkit", importClass "HasChRepr" ]
            , declImportAs "Noodle.Toolkit" [ importValue "empty", importValue "register" ] "Toolkit"
            , declImport "Noodle.Toolkit.Families" [ importType "Families", importType "F", importClass "RegisteredFamily" ]
            , declImport "Cli.Class.CliRenderer" [ importClass "CliRenderer" ]
            ]
            <> (defToModuleImport <$> definitions) <>
            [ declImport opts.streprAt.module_ [ importType opts.streprAt.type_ ]
            , declImport opts.chreprAt.module_ [ importType opts.chreprAt.type_ ]
            ]
            <> opts.imports
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
                        $ (groupBranch <$> definitionsArray)
                        <> [ caseBranch [ binderWildcard ] $ exprString "unknown" ]
                    , binaryOp ">>>" $ exprIdent "Id.unsafeGroupR"
                    ]
            ]
        , declInstance Nothing [] "CliRenderer" [ typeCtor toolkitKey, typeCtor familiesCtor, typeCtor opts.chreprAt.type_, typeVar "m" ]
            [ instValue "cliSize" _5binders $ exprCtor "Nothing"
            , instValue "cliSizeRaw" _5binders $ exprCtor "Nothing"
            , instValue "renderCli" _5binders $ exprApp (exprIdent "pure") [ exprIdent "unit" ]
            , instValue "renderCliRaw" _5binders $ exprApp (exprIdent "pure") [ exprIdent "unit" ]
            ]
        , declInstance Nothing [] "MarkToolkit" [ typeCtor toolkitKey ]
            [ instValue "markGroup" [ binderWildcard, binderVar "group" ]
                $ rgbColorExpr 255 255 255
            , instValue "markFamily" [ binderWildcard, binderWildcard, binderVar "family" ]
                $ rgbColorExpr 255 255 255
            ]
        , generatePossiblyToFnInstance tkName (FCG.Options opts) definitionsArray
        ]
    where
        groupBranch :: Partial => FamilyDef -> _
        groupBranch fdef = caseBranch [ binderString $ Id.family $ FamilyDef.family fdef ] $ exprString $ Id.group $ FamilyDef.group fdef
        _5binders = [ binderWildcard, binderWildcard, binderWildcard, binderWildcard, binderWildcard ]
        rgbColorExpr :: Partial => Int -> Int -> Int -> CST.Expr Void
        rgbColorExpr r g b = exprApp (exprIdent "Color.rgb") [ exprInt r, exprInt g, exprInt b ]
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
            exprOp
                (exprApp (exprIdent "Toolkit.empty")
                    [ exprTyped
                        ( exprCtor "Proxy" )
                        $ typeApp typeWildcard [ typeCtor toolkitKey ]
                    , exprApp (exprIdent "Id.toolkitR")
                        [ exprString $ Id.toolkit tkName ]
                    ]
                )
                (binaryOp "#"
                    <$> exprApp (exprIdent "Toolkit.register")
                    <$> Array.singleton
                    <$> exprIdent
                    <$> referFamily
                    <$> Array.reverse definitions
                )


generatePossiblyToFnInstance :: forall strepr chrepr. Partial => FCG.CodegenRepr chrepr => Toolkit.Name -> FCG.Options strepr chrepr -> Array FamilyDef -> CST.Declaration Void
generatePossiblyToFnInstance tkName (FCG.Options opts) definitionsArray =
    declInstance Nothing [] "PossiblyToFn"
        [ typeCtor toolkitKey
        , typeApp (typeCtor "Maybe") [ typeCtor opts.chreprAt.type_ ]
        , typeApp (typeCtor "Maybe") [ typeCtor opts.chreprAt.type_ ]
        , typeCtor "Id.FamilyR"
        ]
        [ instValue "possiblyToFn" [ binderWildcard ]
            $ exprOp (exprIdent "Id.family")
            [ binaryOp ">>>"
                $ exprCase [ exprSection ]
                    $ (fnDefBranch <$> definitionsArray)
                    <> [ caseBranch [ binderWildcard ] $ exprCtor "Nothing" ]
            ]
        ]
    where
        toolkitKey = String.toUpper $ Id.toolkit tkName -- Id.toolkit tkName <> "Key"
        fnDefBranch :: Partial => FamilyDef -> _
        fnDefBranch fdef =
            case (unwrap $ toFn (Proxy :: _ Void) fdef :: FnS ChannelDef ChannelDef) of
                name /\ inlets /\ outlets ->
                    caseBranch
                        [ binderString name ]
                        $ exprOp ( exprCtor "Just" )
                            [ binaryOp "$"
                                $ exprApp (exprIdent "fn")
                                    [ exprString name
                                    , exprArray $ inletExpr  <$> inlets
                                    , exprArray $ outletExpr <$> outlets
                                    ]
                            ]
        inletExpr :: Partial => Fn.Argument ChannelDef -> CST.Expr Void
        inletExpr chdef = case chdef # Fn.argValue # unwrap of
            { mbType, mbDefault } -> case mbDefault of
                Just _ ->
                    exprOp
                        (exprApp (exprIdent "Fn.in_") [ exprString $ Fn.argName chdef ])
                        [ binaryOp "$" $ qChFullValue mbType mbDefault ]
                Nothing ->
                    exprApp (exprIdent "Fn.inx_") [ exprString $ Fn.argName chdef ]
        outletExpr :: Partial => Fn.Output ChannelDef -> CST.Expr Void
        outletExpr chdef = case chdef # Fn.outValue # unwrap of
            { mbType, mbDefault } -> case mbDefault of
                Just _ ->
                    exprOp
                        (exprApp (exprIdent "Fn.out_") [ exprString $ Fn.outName chdef ])
                        [ binaryOp "$" $ qChFullValue mbType mbDefault ]
                Nothing ->
                    exprApp (exprIdent "Fn.outx_") [ exprString $ Fn.outName chdef ]
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
    Id.toolkit tkName <> "." <> "Toolkit"


toolkitModuleName' :: ModulePrefix -> Toolkit.Name -> String
toolkitModuleName' modPrefix tkName =
    unwrap modPrefix <> "." <> toolkitModuleName tkName


toolkitPath :: GenRootPath -> Toolkit.Name -> String
toolkitPath genRoot tkName = unwrap genRoot <> "/" <> Id.toolkit tkName


toolkitFile :: GenRootPath -> Toolkit.Name -> String
toolkitFile genRoot tkName =
    toolkitPath genRoot tkName <> "/" <> "Toolkit" <> ".purs"