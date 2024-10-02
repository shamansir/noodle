module Noodle.Text.NdfFile.NodeDef.Codegen where

import Prelude

import Data.Array (head, tail, uncons, mapWithIndex) as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..), Replacement(..), replace) as String

import Type.Proxy (Proxy(..))

import Control.Monad.Writer (tell)
import Partial.Unsafe (unsafePartial)

import Effect.Class (liftEffect)

import PureScript.CST.Types
import PureScript.CST.Types (Type, Expr, Declaration) as CST

import Tidy.Codegen hiding (importType, importTypeOp, importValue, importTypeAll)
import Tidy.Codegen.Monad

import Noodle.Fn.Shape.Temperament (Temperament(..)) as T
import Noodle.Fn.Shape.Temperament (Algorithm, defaultAlgorithm, byIndex) as Temperament
import Noodle.Fn.ToFn (Fn)
import Noodle.Fn.ToFn (fn, Fn, toFn, Argument, Output, name, argName, argValue, outName, outValue, arg, out, args, outs) as Fn
import Noodle.Text.NdfFile.Types
import Noodle.Text.NdfFile.NodeDef.ProcessCode (ProcessCode)
import Noodle.Text.NdfFile.NodeDef.ProcessCode (process) as PC


class CodegenRepr :: forall k. k -> Constraint
class CodegenRepr repr where
  reprModule :: Proxy repr -> String
  reprTypeName :: Proxy repr -> String
  reprType :: Proxy repr -> CST.Type Void
  reprDefault :: Proxy repr -> CST.Expr Void
  typeFor :: Proxy repr -> EncodedType -> CST.Type Void
  valueFor :: Proxy repr -> Maybe EncodedType -> EncodedValue -> CST.Expr Void


{- data GenMonad
  = MEffect
  | MAff -}


newtype Options :: forall k. k -> Prim.Type
newtype Options repr = Options
    { temperamentAlgorithm :: Temperament.Algorithm
    , monadAt :: { module_ :: String, type_ :: String }
    , nodeModuleName :: NodeFamily -> String
    , prepr :: Proxy repr
    , imports :: Array (ImportDecl Void)
    }


type Channel = String /\ ChannelDef


__process_stub = "PROCESS GOES HERE" :: String
__process_pattern = "do\n  {- " <> __process_stub <> " -}\n  pure unit" :: String


generate :: forall repr. CodegenRepr repr => Options repr -> FamilyGroup -> StateDef -> Fn ChannelDef ChannelDef -> ProcessCode -> String
generate opts fg state fn process = injectProcess process $ printModule $ generateModule opts fg state fn
  where
    injectProcess processCode = String.replace (String.Pattern __process_pattern) (String.Replacement $ PC.process processCode)


generateModule :: forall repr. CodegenRepr repr => Options repr -> FamilyGroup -> StateDef -> Fn ChannelDef ChannelDef -> Module Void
generateModule (Options opts) (FamilyGroup fGroup) (StateDef state) fn
  = addUserImports_ opts.imports
  $ unsafePartial
  $ codegenModule (opts.nodeModuleName $ NodeFamily $ Fn.name fn) do

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
    familyName = Fn.name fn

    nameOf :: Channel -> String
    nameOf (name /\ _) = name
    qtype :: Maybe EncodedType -> CST.Type Void
    qtype = maybe (reprType opts.prepr :: CST.Type Void) (typeFor opts.prepr)
    qvalue :: Maybe EncodedType -> Maybe EncodedValue -> CST.Expr Void
    qvalue mbDataType = maybe (reprDefault opts.prepr) (valueFor opts.prepr mbDataType)
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
        case Array.uncons $ Array.mapWithIndex inletTypeApp $ Fn.args fn of
            Just { head, tail } ->
                typeParens $ typeOp head $ (binaryOp listOp <$> tail) <> [ binaryOp listOp (typeCtor listTnil) ]
            Nothing -> typeCtor listTnil
    generateOuletsType :: CST.Type Void
    generateOuletsType =
        case Array.uncons $ outletTypeApp <$> Fn.outs fn of
            Just { head, tail } ->
                typeParens $ typeOp head $ (binaryOp listOp <$> tail) <> [ binaryOp listOp (typeCtor listTnil) ]
            Nothing -> typeCtor listTnil

    channelRow :: Channel -> String /\ CST.Type Void
    channelRow chan = nameOf chan /\ typeOf chan

    channelDefault :: Channel -> String /\ CST.Expr Void
    channelDefault chan = nameOf chan /\ defaultOf chan

    inletDeclr :: Channel -> CST.Declaration Void
    inletDeclr chan =
      declValue (nameOf chan <> "_in") []
        $ exprTyped
          (exprCtor shapeN.inlet)
          (typeApp
            typeWildcard
            [ typeString $ nameOf chan ]
          )

    outletDeclr :: Channel -> CST.Declaration Void
    outletDeclr chan =
      declValue (nameOf chan <> "_out") []
        $ exprTyped
          (exprCtor shapeN.outlet)
          (typeApp
            typeWildcard
            [ typeString $ nameOf chan ]
          )


  tell $
    [ declSignature ("_" <> familyName) $ typeApp (typeCtor familyIdCtor) [ typeString familyName ]
    , declValue ("_" <> familyName) [] $ exprCtor familyIdCtor

    , declType "Inlets" []
        $ typeKinded generateInletsType
        $ typeCtor shapeN.inlets

    , declType "Outlets" []
        $ typeKinded generateOuletsType
        $ typeCtor shapeN.outlets

    , declType "InletsRow" []
        $ typeRow (channelRow <$> Fn.args fn)
        Nothing

    , declType "OutletsRow" []
        $ typeRow (channelRow <$> Fn.outs fn)
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
        $ exprRecord $ channelDefault <$> Fn.args fn

    , declSignature "defaultO" $ typeApp (typeCtor "Record") [ typeCtor "OutletsRow" ]
    , declValue "defaultO" []
        $ exprRecord $ channelDefault <$> Fn.outs fn

    ]
    <> (inletDeclr <$> Fn.args fn)
    <> (outletDeclr <$> Fn.outs fn)
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
