module Blessed.Internal.Core where

import Prelude (Unit, (<<<), map)

import Effect (Effect)
import Blessed.Command (Command)

import Data.Symbol (reflectSymbol, class IsSymbol)
import Type.Proxy (Proxy(..))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)


newtype NodeId = NodeId String


data Prop :: Row Type -> Type
data Prop (r :: Row Type) = Prop String Json


data Handler m e = Handler e (BlessedOp m Unit)

data SProp = SProp String Json
data SNode m e = SNode NodeId (Array SProp) (Array (SNode m e)) (Array (Handler m e))


type Blessed m e = SNode m e


data BlessedOp m a
    = Lift (m a)
    | PerformOne NodeId Command a
    | PerformSome NodeId (Array Command) a


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (r :: Row Type) m e = Array (Prop r) -> Array (Blessed m e) -> Blessed m e
type Leaf (r :: Row Type) m e = Array (Prop r) -> Blessed m e



lockProps :: forall r. Array (Prop r) -> Array SProp
lockProps = map lockOne
    where lockOne (Prop str json) = SProp str json


prop :: forall (sym :: Symbol) (r :: Row Type) a. IsSymbol sym => EncodeJson a => Proxy sym -> a -> Prop r
prop sym = Prop (reflectSymbol sym) <<< encodeJson


foreign import execute_ :: forall m e. Blessed m e -> Effect Unit