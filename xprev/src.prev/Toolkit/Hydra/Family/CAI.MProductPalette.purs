module Toolkit.Hydra.Family.CAI.FProductPalette where


import Prelude

import Data.SOrder (SOrder, type (:::), T, s1, s3)
import Data.Maybe (Maybe(..))
import Data.Int (fromNumber, toNumber) as Int
import Data.Array (index) as Array

import Control.Monad.State as State

import Noodle.Fn as Fn
import Noodle.Id (Input(..), Output(..)) as Fn
import Noodle.Fn.Process as P
import Noodle.Family.Def as Family
import Noodle.Node (Node) as N
import Noodle.Id (Family(..)) as Node

import Type.Proxy (Proxy(..))

import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap (WrapRepr)

import CompArts.Product as CAI


id = Node.Family :: _ "caiProductPalette"


name :: String
name = "caiProductPalette"


type State = CAI.Products


defaultState :: State
defaultState = CAI.none


_in_product    = Fn.Input  1 :: _ "product"

_out_primary   = Fn.Output 0 :: _ "primary"
_out_secondary = Fn.Output 1 :: _ "secondary"
_out_ternary   = Fn.Output 2 :: _ "ternary"


type Inputs = ( product :: CAI.Product' )
type Outputs = ( primary :: H.Texture, secondary :: H.Texture, ternary :: H.Texture )


inputsOrder :: _
inputsOrder = s1 _in_product


outputsOrder :: _
outputsOrder = s3 _out_primary _out_secondary _out_ternary


defaultInputs :: Record Inputs
defaultInputs = { product : CAI.defaultProduct' }


defaultOutputs :: Record Outputs
defaultOutputs = { primary : H.Empty, secondary : H.Empty, ternary : H.Empty  }


type Family (m :: Type -> Type) = -- {-> caiProductPalette <-}
    Family.Def State
        Inputs
        Outputs
        WrapRepr
        m


family :: forall (m :: Type -> Type). Family m
family = -- {-> caiProductPalette <-}
    Family.def
        defaultState
        defaultInputs
        defaultOutputs
        $ Fn.make name
            { inputs : inputsOrder, outputs : outputsOrder }
            $ do
            productV <- P.receive _in_product
            case productV of
                CAI.Product' (-1) _ -> pure unit
                CAI.Product' _ product -> do
                    case loadPalette product.palette of
                        Just { primary, secondary, ternary } -> do
                            P.send _out_primary $ toTexture primary
                            P.send _out_secondary $ toTexture secondary
                            P.send _out_ternary $ toTexture ternary
                        Nothing -> do
                            P.send _out_primary $ H.Empty
                            P.send _out_secondary H.Empty
                            P.send _out_ternary H.Empty
            pure unit
    where
        toPalette primary secondary ternary =
            { primary, secondary, ternary }
        loadPalette arr =
            toPalette <$> Array.index arr 0 <*> Array.index arr 1 <*> Array.index arr 2
        toTexture { r, g, b } =
            H.Start $ H.Solid
                { r : H.Number $ Int.toNumber r / 255.0
                , g : H.Number $ Int.toNumber g / 255.0
                , b : H.Number $ Int.toNumber b / 255.0
                , a : H.Number 1.0
                }


type Node (m :: Type -> Type) =
    N.Node "caiProductPalette" State
        Inputs
        Outputs
        WrapRepr
        m