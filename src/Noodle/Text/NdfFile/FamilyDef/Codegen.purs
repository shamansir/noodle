module Noodle.Text.NdfFile.FamilyDef.Codegen where

import Prelude

import Data.Array (mapWithIndex, uncons) as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..), Replacement(..), replace) as String
import Data.String.Extra (pascalCase) as String

import Type.Proxy (Proxy)

import Control.Monad.Writer (tell)
import Partial.Unsafe (unsafePartial)

import PureScript.CST.Types (ImportDecl, Module(..))
import PureScript.CST.Types (Type, Expr, Declaration) as CST

import Tidy.Codegen hiding (importType, importTypeOp, importValue, importTypeAll)
import Tidy.Codegen.Monad (codegenModule, importFrom, importOpen, importType, importTypeAll, importTypeOp, importValue)

import Noodle.Id as Id
import Noodle.Fn.Shape.Temperament (Temperament(..)) as T
import Noodle.Fn.Shape.Temperament (Algorithm, byIndex) as Temperament
import Noodle.Fn.ToFn (Fn(..))
import Noodle.Fn.ToFn (Argument, Output, args, name, outs, argName, argValue, outName, outValue) as Fn
import Noodle.Text.FromCode (Source) as FC
import Noodle.Text.NdfFile.Types (Source, FamilyDefRec, ChannelDef(..), EncodedType, EncodedValue, StateDef(..), familyOf)
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode)
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (process) as PC


class ValueCodegen a where
  mkExpression :: a -> CST.Expr Void


class TypeCodegen a where
  mkType :: a -> CST.Type Void


instance Partial => ValueCodegen Unit    where mkExpression = const $ exprIdent "unit"
instance Partial => ValueCodegen Int     where mkExpression = exprInt
instance Partial => ValueCodegen Number  where mkExpression = exprNumber
instance Partial => ValueCodegen Boolean where mkExpression = exprBool
instance Partial => ValueCodegen Char    where mkExpression = exprChar
instance Partial => ValueCodegen String  where mkExpression = exprString
instance ( Partial, ValueCodegen a ) => ValueCodegen (Array a)
  where mkExpression items = exprArray $ mkExpression <$> items
instance
  ( Partial, ValueCodegen a, ValueCodegen b )
  => ValueCodegen (a /\ b)
  where
  mkExpression (a /\ b) =
    exprOp (mkExpression a)
      [ binaryOp "/\\" $ mkExpression b
      ] -- FIXME: relies on the imported operator


instance (Partial, ValueCodegen a) => ValueCodegen (Maybe a) where
  mkExpression = case _ of
    Just a -> exprApp (exprCtor "Just") [ mkExpression a ]
    Nothing -> exprCtor "Nothing"


instance (Partial, ValueCodegen val) => ValueCodegen (Fn.Argument val) where
  mkExpression arg =
    exprApp (exprIdent "Fn.arg") [ exprString $ Fn.argName arg, mkExpression $ Fn.argValue arg ]


instance (Partial, ValueCodegen val) => ValueCodegen (Fn.Output val) where
  mkExpression arg =
    exprApp (exprIdent "Fn.out") [ exprString $ Fn.outName arg, mkExpression $ Fn.outValue arg ]


instance (Partial, ValueCodegen arg, ValueCodegen out) => ValueCodegen (Fn arg out) where
  mkExpression = case _ of
    Fn (name /\ args /\ outs) ->
      exprApp (exprCtor "Fn")
        [ exprOp (exprString name)
          [ binaryOp "/\\" (exprArray $ mkExpression <$> args)
          , binaryOp "/\\" (exprArray $ mkExpression <$> outs)
          ]
        ]


class CodegenRepr :: forall k. k -> Constraint
class CodegenRepr repr where
  reprModule :: Proxy repr -> String -- a full path to the module where repr wrapper is located
  reprTypeName :: Proxy repr -> String -- a name of the type of the repr wrapper itself
  reprType :: Proxy repr -> CST.Type Void -- a CST type represntation of the repr wrapper itself
  typeFor :: Proxy repr -> EncodedType -> CST.Type Void -- a CST type representation for diven encoded type
  defaultFor :: Proxy repr -> Maybe EncodedType -> CST.Expr Void -- default value for the given type (if specified) in case when expected default value wasn't provided by user in the code
  valueFor :: Proxy repr -> Maybe EncodedType -> EncodedValue -> CST.Expr Void -- a CST value representation for a given type (if specified) and given encoded value


{- data GenMonad
  = MEffect
  | MAff -}


type OptionsRec :: forall k. k -> Type
type OptionsRec repr =
  { temperamentAlgorithm :: Temperament.Algorithm
  , monadAt :: { module_ :: String, type_ :: String }
  , familyModuleName :: Id.GroupR -> Id.FamilyR -> String -- FIXME: just ModulePrefix
  , infoComment :: Maybe (Maybe Source -> Id.GroupR -> Id.FamilyR -> String)
  , prepr :: Proxy repr
  , reprAt :: { module_ :: String, type_ :: String }
  , imports :: Array (ImportDecl Void)
  }


newtype Options :: forall k. k -> Prim.Type
newtype Options repr = Options (OptionsRec repr)


type Channel = String /\ ChannelDef


__process_stub = "PROCESS GOES HERE" :: String
__process_pattern = "do\n  {- " <> __process_stub <> " -}\n  pure unit" :: String
__default_info_comment = "Generated by Noodle NDF Codegen" :: String


generate :: forall repr. CodegenRepr repr => Options repr -> Maybe Source -> FamilyDefRec -> String
generate opts mbSource fdef = injectProcess fdef.process $ printModule $ generateModule opts mbSource fdef
  where
    injectProcess processCode = String.replace (String.Pattern __process_pattern) (String.Replacement $ PC.process processCode)


generateModule :: forall repr. CodegenRepr repr => Options repr -> Maybe Source -> FamilyDefRec -> Module Void
generateModule (Options opts) mbSource fdef
  = let (StateDef state) = fdef.state in
  addUserImports_ opts.imports
  $ unsafePartial
  $ codegenModule (opts.familyModuleName fdef.group $ familyOf fdef) do

  importOpen "Prelude"
  nodeMonad <- importFrom opts.monadAt.module_ $ importType opts.monadAt.type_ -- FIXME: use forall
  listOp <- importFrom "Type.Data.List" $ importTypeOp ":>"
  listTnil <- importFrom "Type.Data.List.Extra" $ importType "TNil"
  familyIdCtor <- importFrom "Noodle.Id" $ importTypeAll "NId.Family"
  temper <- importFrom "Noodle.Fn.Shape.Temperament" $
    { hot : importType "Hot"
    , cold : importType "Cold"
    }
  shape <- importFrom "Noodle.Fn.Shape" $
    { i : importType "I"
    , o : importType "O"
    }
  shapeN <- importFrom "Noodle.Fn.Shape" $
    { shape : importTypeAll "Noodle.Shape"
    , inlets : importType "Noodle.Inlets"
    , outlets : importType "Noodle.Outlets"
    , inlet : importTypeAll "Noodle.Inlet"
    , outlet : importTypeAll "Noodle.Outlet"
    }
  processN <- importFrom "Noodle.Fn.Process" $ importType "Noodle.Process"
  _ <- importFrom "Noodle.Fn.Process" $
    { receive : importValue "Fn.receive"
    , send : importValue "Fn.send"
    }
  nodeCtor <- importFrom "Noodle.Node" $ importType "Noodle.Node"
  tkFamilyN <- importFrom "Noodle.Toolkit.Family" $ importType "Noodle.Family"
  tkFamilyF <- importFrom "Noodle.Toolkit.Family" $
    { make : importValue "Family.make"
    , spawn : importValue "Family.spawn"
    }
  tkF <- importFrom "Noodle.Toolkit.Families" $ importType "Noodle.F"
  reprC <- importFrom (reprModule opts.prepr) $ importType $ reprTypeName opts.prepr -- FIXME: use forall?

  let
    familyName = Fn.name fdef.fn

    nameOf :: Channel -> String
    nameOf (name /\ _) = name
    qtype :: Maybe EncodedType -> CST.Type Void
    qtype = maybe (reprType opts.prepr :: CST.Type Void) (typeFor opts.prepr)
    qvalue :: Maybe EncodedType -> Maybe EncodedValue -> CST.Expr Void
    qvalue mbDataType = maybe (defaultFor opts.prepr mbDataType) (valueFor opts.prepr mbDataType)
    typeOf :: Channel -> CST.Type Void
    typeOf (_ /\ ChannelDef { mbType }) = qtype mbType
    defaultOf :: Channel -> CST.Expr Void
    defaultOf (_ /\ ChannelDef { mbType, mbDefault }) = qvalue mbType mbDefault

    inletTypeApp :: Int -> Channel-> CST.Type Void
    inletTypeApp idx chan =
        typeApp (typeCtor shape.i)
            [ typeString $ nameOf chan
            , typeCtor $ case Temperament.byIndex opts.temperamentAlgorithm idx of
                T.Hot -> temper.hot
                T.Cold -> temper.cold
            , typeOf chan
            ]
    outletTypeApp :: Channel -> CST.Type Void
    outletTypeApp chan =
        typeApp (typeCtor shape.o)
            [ typeString $ nameOf chan
            , typeOf chan
            ]

    generateInletsType :: CST.Type Void
    generateInletsType =
        case Array.uncons $ Array.mapWithIndex inletTypeApp $ Fn.args fdef.fn of
            Just { head, tail } ->
                typeParens $ typeOp head $ (binaryOp listOp <$> tail) <> [ binaryOp listOp (typeCtor listTnil) ]
            Nothing -> typeCtor listTnil
    generateOuletsType :: CST.Type Void
    generateOuletsType =
        case Array.uncons $ outletTypeApp <$> Fn.outs fdef.fn of
            Just { head, tail } ->
                typeParens $ typeOp head $ (binaryOp listOp <$> tail) <> [ binaryOp listOp (typeCtor listTnil) ]
            Nothing -> typeCtor listTnil

    channelRow :: Channel -> String /\ CST.Type Void
    channelRow chan = nameOf chan /\ typeOf chan

    channelDefault :: Channel -> String /\ CST.Expr Void
    channelDefault chan = nameOf chan /\ defaultOf chan

    inletDeclr :: Channel -> CST.Declaration Void
    inletDeclr chan =
      declValue ("_in_" <> nameOf chan) []
        $ exprTyped
          (exprCtor shapeN.inlet)
          (typeApp
            typeWildcard
            [ typeString $ nameOf chan ]
          )

    outletDeclr :: Channel -> CST.Declaration Void
    outletDeclr chan =
      declValue ("_out_" <> nameOf chan) []
        $ exprTyped
          (exprCtor shapeN.outlet)
          (typeApp
            typeWildcard
            [ typeString $ nameOf chan ]
          )

    infoComment :: String
    infoComment = maybe __default_info_comment (\f -> f mbSource fdef.group $ familyOf fdef) opts.infoComment


  tell $
    [ leading (lineBreaks 2 <> blockComment infoComment <> lineBreaks 2)
      $ declSignature ("_" <> familyName) $ typeApp (typeCtor familyIdCtor) [ typeString familyName ]
    , declValue ("_" <> familyName) [] $ exprCtor familyIdCtor

    , declType "Inlets" []
        $ typeKinded generateInletsType
        $ typeCtor shapeN.inlets

    , declType "Outlets" []
        $ typeKinded generateOuletsType
        $ typeCtor shapeN.outlets

    , declType "InletsRow" []
        $ typeRow (channelRow <$> Fn.args fdef.fn)
        Nothing

    , declType "OutletsRow" []
        $ typeRow (channelRow <$> Fn.outs fdef.fn)
        Nothing

    , declType "Shape" []
        $ typeApp (typeCtor shapeN.shape)
            [ typeCtor "Inlets", typeCtor "Outlets" ]

    , declType "Process" []
        $ typeApp (typeCtor processN)
            [ qtype state.mbType
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor nodeMonad
            ]

    , declType "Node" []
        $ typeApp (typeCtor nodeCtor)
            [ typeString familyName
            , qtype state.mbType
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor nodeMonad
            ]

    , declType "Family" []
        $ typeApp (typeCtor tkFamilyN)
            [ typeString familyName
            , qtype state.mbType
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor nodeMonad
            ]

    , declType "F" []
        $ typeApp (typeCtor tkF)
            [ typeString familyName
            , qtype state.mbType
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor nodeMonad
            ]

    , declSignature "defaultI" $ typeApp (typeCtor "Record") [ typeCtor "InletsRow" ]
    , declValue "defaultI" []
        $ exprRecord $ channelDefault <$> Fn.args fdef.fn

    , declSignature "defaultO" $ typeApp (typeCtor "Record") [ typeCtor "OutletsRow" ]
    , declValue "defaultO" []
        $ exprRecord $ channelDefault <$> Fn.outs fdef.fn

    ]
    <> (inletDeclr <$> Fn.args fdef.fn)
    <> (outletDeclr <$> Fn.outs fdef.fn)
    <>
    [ declSignature "family" $ typeCtor "Family"
    , declValue "family" []
        $ exprApp
          (exprIdent tkFamilyF.make)
          [ exprIdent $ "_" <> familyName
          , qvalue state.mbType state.mbDefault
          , exprParens (exprTyped (exprCtor "Noodle.Shape") (typeCtor "Shape"))
          , exprIdent "defaultI"
          , exprIdent "defaultO"
          , exprIdent $ familyName <> "P"
          ]

    , declSignature "makeNode" $ typeApp (typeCtor nodeMonad) [ typeCtor "Node" ]
    , declValue "makeNode" []
        $ exprApp
          (exprIdent tkFamilyF.spawn)
          [ exprIdent "family"
          ]

    , declSignature (familyName <> "P") $ typeCtor "Process"
    , declValue (familyName <> "P") []
        $ exprDo
          [ ]
          (leading (lineBreaks 1 <> blockComment __process_stub <> lineBreaks 1) $ exprApp (exprIdent "pure") [ exprIdent "unit" ])
    ]


addUserImports_ :: Array (ImportDecl Void) -> Module Void -> Module Void
addUserImports_ uimports (Module { header, body }) =
  Module
    { header :
        unwrap header
          # \rec -> rec { imports = rec.imports <> uimports }
          # wrap
    , body
    }


familyPascalCase :: Id.FamilyR -> String
familyPascalCase = Id.family >>> String.pascalCase


groupPascalCase :: Id.GroupR -> String
groupPascalCase = Id.group >>> String.pascalCase


withOptions :: forall repr. Options repr -> (OptionsRec repr -> OptionsRec repr) -> Options repr
withOptions (Options rec) f = Options $ f rec