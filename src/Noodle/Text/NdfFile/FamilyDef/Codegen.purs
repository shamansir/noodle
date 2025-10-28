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

import Tidy.Codegen hiding (importType, importTypeOp, importValue, importTypeAll, importClass)
import Tidy.Codegen.Class (class OverLeadingComments)
import Tidy.Codegen.Monad (codegenModule, importFrom, importOpen, importType, importTypeAll, importTypeOp, importValue, importClass)

import Noodle.Id as Id
import Noodle.Fn.Shape.Temperament (Temperament(..)) as T
import Noodle.Fn.Shape.Temperament (Algorithm, byIndex) as Temperament
import Noodle.Fn.Signature (Signature(..))
import Noodle.Fn.Signature (Argument, Output, args, name, outs, argName, argValue, outName, outValue) as Sig
import Noodle.Text.FromCode (Source) as FC
import Noodle.Text.NdfFile.Types (Source, FamilyDefRec, ChannelDef(..), EncodedType, EncodedValue, StateDef(..), familyOf)
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode)
import Noodle.Text.NdfFile.FamilyDef.ProcessCode (ProcessCode(..), process, Indent(..)) as PC
import Noodle.Toolkit (Name) as Toolkit


class ValueCodegen a where
  mkExpression :: a -> CST.Expr Void


class TypeCodegen a where
  mkType :: a -> CST.Type Void


class ValueEncode a where
  encodeValue :: a -> Maybe EncodedValue


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


instance (Partial, ValueCodegen val) => ValueCodegen (Sig.Argument val) where
  mkExpression arg =
    exprApp (exprIdent "Fn.arg") [ exprString $ Sig.argName arg, mkExpression $ Sig.argValue arg ]


instance (Partial, ValueCodegen val) => ValueCodegen (Sig.Output val) where
  mkExpression arg =
    exprApp (exprIdent "Fn.out") [ exprString $ Sig.outName arg, mkExpression $ Sig.outValue arg ]


instance (Partial, ValueCodegen arg, ValueCodegen out) => ValueCodegen (Signature arg out) where
  mkExpression = case _ of
    Sig (name /\ args /\ outs) ->
      exprApp (exprCtor "Sig")
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
  fTypeFor :: Proxy repr -> Maybe EncodedType -> CST.Type Void -- a CST type representation for given encoded type (full, from root type)
  fDefaultFor :: Proxy repr -> Maybe EncodedType -> CST.Expr Void -- default value for the given type (if specified) in case when expected default value wasn't provided by user in the code (full, from root type)
  fValueFor :: Proxy repr -> Maybe EncodedType -> EncodedValue -> CST.Expr Void -- a CST value representation for a given type (if specified) and given encoded value (full, from root type)
  pTypeFor :: Proxy repr -> Maybe EncodedType -> CST.Type Void -- a CST type representation for given encoded type (partial, from inner type)
  pDefaultFor :: Proxy repr -> Maybe EncodedType -> CST.Expr Void -- default value for the given type (if specified) in case when expected default value wasn't provided by user in the code (partial, from inner type)
  pValueFor :: Proxy repr -> Maybe EncodedType -> EncodedValue -> CST.Expr Void -- a CST value representation for a given type (if specified) and given encoded value (partial, from inner type)


class ParseableRepr a where -- FIXME: use `ParseableRepr` + `ValueCodegen` + `TypeCodegen` in `CodegenRepr`
  toDefault :: EncodedType -> a
  toRepr :: EncodedType -> EncodedValue -> Maybe a


{- data GenMonad
  = MEffect
  | MAff -}


type OptionsRec :: forall k. k -> k -> Type
type OptionsRec strepr chrepr =
  { temperamentAlgorithm :: Temperament.Algorithm
  , monadAt :: { module_ :: String, type_ :: String }
  , familyModuleName :: Id.GroupR -> Id.FamilyR -> String -- FIXME: just ModulePrefix
  , toolkitModuleName :: Toolkit.Name -> String -- FIXME: just ModulePrefix, don't mix `FamilyDef` generation with `Toolkit` generation
  , infoComment :: Maybe (Maybe Source -> Id.GroupR -> Id.FamilyR -> String)
  , pstrepr :: Proxy strepr
  , pchrepr :: Proxy chrepr
  , chreprAt :: { module_ :: String, type_ :: String }
  , streprAt :: { module_ :: String, type_ :: String }
  , pstreprType :: String
  , tkImports :: Array (ImportDecl Void)
  , familyImports :: Id.FamilyR -> Array (ImportDecl Void)
  }


newtype Options :: forall k. k -> k -> Prim.Type
newtype Options strepr chrepr = Options (OptionsRec strepr chrepr)


type Channel = String /\ ChannelDef


__process_indent = "  " :: String
__process_stub = "PROCESS GOES HERE" :: String
__process_pattern = "do\n" <> __process_indent <> "{- " <> __process_stub <> " -}\n" <> __process_indent <> "pure unit" :: String
__process_expr :: Partial => OverLeadingComments Void => CST.Expr Void
__process_expr = exprDo [ ] (leading (lineBreaks 1 <> blockComment __process_stub <> lineBreaks 1) $ exprApp (exprIdent "pure") [ exprIdent "unit" ])

__raw_process_indent = "        " :: String
__raw_process_indent_x = __raw_process_indent <> "  " :: String
__raw_process_stub :: Id.FamilyR -> String
__raw_process_stub familyR = Id.family familyR <> " :: PROCESS GOES HERE"
__raw_process_pattern :: Id.FamilyR -> String
__raw_process_pattern familyR = "\n" <> __raw_process_indent <> "do\n" <> __raw_process_indent_x <> "{- " <> __raw_process_stub familyR <> " -}\n" <> __raw_process_indent_x <> "pure unit" :: String
__raw_process_expr :: Partial => OverLeadingComments Void => Id.FamilyR -> CST.Expr Void
__raw_process_expr familyR =
    leading (lineBreaks 1)
        $ exprDo [ ]
            (leading (lineBreaks 1 <> blockComment (__raw_process_stub familyR) <> lineBreaks 1)
            $ exprApp (exprIdent "pure") [ exprIdent "unit" ])

__default_info_comment = "Generated by Noodle NDF Codegen" :: String


generate :: forall strepr chrepr. CodegenRepr strepr => CodegenRepr chrepr => Options strepr chrepr -> Maybe Source -> FamilyDefRec -> String
generate opts mbSource fdef = injectProcess fdef.process $ printModule $ generateModule opts mbSource fdef
  where
    injectProcess processCode = String.replace (String.Pattern __process_pattern) (String.Replacement $ PC.process (PC.Indent __process_indent) processCode)


generateModule :: forall strepr chrepr. CodegenRepr strepr => CodegenRepr chrepr => Options strepr chrepr -> Maybe Source -> FamilyDefRec -> Module Void
generateModule (Options opts) mbSource fdef
  = let (StateDef state) = fdef.state in
  addUserImports_ (opts.familyImports $ familyOf fdef)
  $ unsafePartial
  $ codegenModule (opts.familyModuleName fdef.group $ familyOf fdef) do

  importOpen "Prelude"
  nodeMonad <- importFrom opts.monadAt.module_ $ importType opts.monadAt.type_ -- FIXME: use forall
  listOp <- importFrom "Type.Data.List" $ importTypeOp ":>"
  listTnil <- importFrom "Type.Data.List.Extra" $ importType "TNil"
  newtypeClass <- importFrom "Data.Newtype" $ importClass "Newtype"
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
  hasFallback <- importFrom "Noodle.Repr.HasFallback" $ importClass "HasFallback"
  reprC <- importFrom (reprModule opts.pchrepr) $ importTypeAll $ reprTypeName opts.pchrepr -- FIXME: use forall?
  -- reprS <- importFrom (reprModule opts.pstrepr) $ importTypeAll $ reprTypeName opts.pstrepr -- FIXME: use forall?

  case fdef.process of
    PC.JS _ -> do
      _ <- importFrom "Noodle.Raw.Fn.Process" $ importValue "jsCode"
      _ <- importFrom "Noodle.Raw.Fn.Process" $ importValue "fromJsCode"
      pure unit
    _ -> pure unit

  let
    familyName = Sig.name fdef.fnsig

    nameOf :: Channel -> String
    nameOf (name /\ _) = name

    qPartialReprType :: Maybe EncodedType -> CST.Type Void
    qPartialReprType = pTypeFor opts.pchrepr
    qPartialReprValue :: Maybe EncodedType -> Maybe EncodedValue -> CST.Expr Void
    qPartialReprValue mbDataType = maybe (pDefaultFor opts.pchrepr mbDataType) (pValueFor opts.pchrepr mbDataType)

    qPartialStateType :: Maybe EncodedType -> CST.Type Void
    qPartialStateType = pTypeFor opts.pstrepr
    qPartialStateValue :: Maybe EncodedType -> Maybe EncodedValue -> CST.Expr Void
    qPartialStateValue mbDataType = maybe (pDefaultFor opts.pstrepr mbDataType) (pValueFor opts.pstrepr mbDataType)

    chPartialTypeOf :: Channel -> CST.Type Void
    chPartialTypeOf (_ /\ ChannelDef { mbType }) = qPartialReprType mbType
    chPartialDefaultOf :: Channel -> CST.Expr Void
    chPartialDefaultOf (_ /\ ChannelDef { mbType, mbDefault }) = qPartialReprValue mbType mbDefault

    inletTypeApp :: Int -> Channel-> CST.Type Void
    inletTypeApp idx chan =
        typeApp (typeCtor shape.i)
            [ typeString $ nameOf chan
            , typeCtor $ case Temperament.byIndex opts.temperamentAlgorithm idx of
                T.Hot -> temper.hot
                T.Cold -> temper.cold
            , chPartialTypeOf chan
            ]
    outletTypeApp :: Channel -> CST.Type Void
    outletTypeApp chan =
        typeApp (typeCtor shape.o)
            [ typeString $ nameOf chan
            , chPartialTypeOf chan
            ]

    generateInletsType :: CST.Type Void
    generateInletsType =
        case Array.uncons $ Array.mapWithIndex inletTypeApp $ Sig.args fdef.fnsig of
            Just { head, tail } ->
                typeParens $ typeOp head $ (binaryOp listOp <$> tail) <> [ binaryOp listOp (typeCtor listTnil) ]
            Nothing -> typeCtor listTnil
    generateOuletsType :: CST.Type Void
    generateOuletsType =
        case Array.uncons $ outletTypeApp <$> Sig.outs fdef.fnsig of
            Just { head, tail } ->
                typeParens $ typeOp head $ (binaryOp listOp <$> tail) <> [ binaryOp listOp (typeCtor listTnil) ]
            Nothing -> typeCtor listTnil

    channelRow :: Channel -> String /\ CST.Type Void
    channelRow chan = nameOf chan /\ chPartialTypeOf chan

    channelDefault :: Channel -> String /\ CST.Expr Void
    channelDefault chan = nameOf chan /\ chPartialDefaultOf chan

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
        $ typeRow (channelRow <$> Sig.args fdef.fnsig)
        Nothing

    , declType "OutletsRow" []
        $ typeRow (channelRow <$> Sig.outs fdef.fnsig)
        Nothing

    , declType "Shape" []
        $ typeApp (typeCtor shapeN.shape)
            [ typeCtor "Inlets", typeCtor "Outlets" ]

    , declNewtype "State" [] "State"
        $ qPartialStateType state.mbType

    , declType "Process" []
        $ typeApp (typeCtor processN)
            [ typeCtor "State"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor nodeMonad
            ]

    , declType "Node" []
        $ typeApp (typeCtor nodeCtor)
            [ typeString familyName
            , typeCtor "State"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor nodeMonad
            ]

    , declType "Family" []
        $ typeApp (typeCtor tkFamilyN)
            [ typeString familyName
            , typeCtor "State"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor nodeMonad
            ]

    , declType "F" []
        $ typeApp (typeCtor tkF)
            [ typeString familyName
            , typeCtor "State"
            , typeCtor "InletsRow"
            , typeCtor "OutletsRow"
            , typeCtor reprC
            , typeCtor nodeMonad
            ]

    , declSignature "defaultI" $ typeApp (typeCtor "Record") [ typeCtor "InletsRow" ]
    , declValue "defaultI" []
        $ exprRecord $ channelDefault <$> Sig.args fdef.fnsig

    , declSignature "defaultO" $ typeApp (typeCtor "Record") [ typeCtor "OutletsRow" ]
    , declValue "defaultO" []
        $ exprRecord $ channelDefault <$> Sig.outs fdef.fnsig

    , declSignature "defaultSt" $ typeCtor "State"
    , declValue "defaultSt" [] $ exprApp (exprCtor "State")
        [ qPartialStateValue state.mbType state.mbDefault ]
    ]
    <> (inletDeclr  <$> Sig.args fdef.fnsig)
    <> (outletDeclr <$> Sig.outs fdef.fnsig)
    <>
    [ declSignature "family" $ typeCtor "Family"
    , declValue "family" []
        $ exprApp
          (exprIdent tkFamilyF.make)
          [ exprIdent $ "_" <> familyName
          , exprIdent "defaultSt"
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
    , declValue (familyName <> "P") [] __process_expr
    , declInstance Nothing [] hasFallback [ typeCtor "State" ]
        [ instValue "fallback" [] $ exprIdent "defaultSt" ]
    , declDerive Nothing [] newtypeClass [ typeCtor "State", typeWildcard ]
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


withOptions :: forall strepr chrepr. Options strepr chrepr -> (OptionsRec strepr chrepr -> OptionsRec strepr chrepr) -> Options strepr chrepr
withOptions (Options rec) f = Options $ f rec