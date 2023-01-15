module Blessed.Internal.Core where

-- export BlessedOp from here

import Prelude

import Effect (Effect)
import Data.Either (Either)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))

import Data.Symbol (reflectSymbol, class IsSymbol)
import Type.Proxy (Proxy(..))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

import Blessed.Internal.BlessedOp as I
import Blessed.Internal.Command as I
import Data.Identity (Identity)


type NodeId = I.NodeId


data Prop :: Row Type -> (Type -> Type) -> Type -> Type
data Prop (r :: Row Type) m e
    = Prop String Json
    | Handler e (NodeId -> Json -> I.BlessedOp m)


data OnlyProp (r :: Row Type)
    = OnlyProp String Json


-- newtype OnlyProp r = OnlyProp (Prop r Identity Unit)


instance Functor (Prop r m) where
    map f (Handler e op) = Handler (f e) op
    map _ (Prop str json) = Prop str json


type Blessed m e = I.SNode m e


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (r :: Row Type) m e = Events e => Array (Prop r m e) -> Array (Blessed m e) -> Blessed m CoreEvent
type NodeAnd (r :: Row Type) m e = Array (Prop r m e) -> Array (Blessed m e) -> (NodeId -> I.BlessedOp m) -> Blessed m CoreEvent
type Leaf (r :: Row Type) m e = Array (Prop r m e) -> Blessed m CoreEvent
type LeafAnd (r :: Row Type) m e = Array (Prop r m e) ->  I.BlessedOp m -> Blessed m CoreEvent
type Handler (r :: Row Type) m e = (NodeId -> Json -> I.BlessedOp m) -> Prop r m e


splitProps :: forall r m e. Array (Prop r m e) -> Array I.SProp /\ Array (I.SHandler m e)
splitProps props = Array.catMaybes (lockSProp <$> props) /\ Array.catMaybes (lockSHandler <$> props)
    where
        lockSProp (Prop str json) = Just $ I.SProp str json
        lockSProp _ = Nothing
        lockSHandler (Handler e op) = Just $ I.SHandler e op
        lockSHandler _ = Nothing


-- FIXME: no `Cons` check here, but only above
prop :: forall (sym :: Symbol) (r :: Row Type) a m e. IsSymbol sym => EncodeJson a => Proxy sym -> a -> Prop r m e
prop sym = Prop (reflectSymbol sym) <<< encodeJson


onlyProp :: forall (sym :: Symbol) (r :: Row Type) a. IsSymbol sym => EncodeJson a => Proxy sym -> a -> OnlyProp r
onlyProp sym = OnlyProp (reflectSymbol sym) <<< encodeJson


handler :: forall r m e. Events e => e -> Handler r m e
handler = Handler


node :: forall r m e. String -> Node r m e
node name props children =
    I.SNode (I.NodeId name) sprops (map toCore <$> children) (map toCore <$> handlers)
    where sprops /\ handlers = splitProps props


data CoreEvent =
    CoreEvent


class Events e where
    initial :: e
    convert :: e -> String
    toCore :: e -> CoreEvent
    fromCore :: CoreEvent -> Maybe e
    -- extract :: e -> Json -> Json


instance Events CoreEvent where
    initial = CoreEvent
    convert _ = "Core"
    toCore = identity
    fromCore = Just
    -- extract _ = identity



nodeAnd :: forall r m e. Events e => String -> NodeAnd r m e
nodeAnd name props children fn =
    I.SNode (I.NodeId name) sprops (map toCore <$> children) (I.SHandler initial (\id _ -> fn id) : (map toCore <$> handlers))
    where sprops /\ handlers = splitProps props



type PropJson = { name :: String, value :: Json }


propCodec :: CA.JsonCodec PropJson
propCodec =
    CA.object "OnlyProp"
        (CAR.record
            { name : CA.string
            , value : CA.json
            }
        )


instance EncodeJson (OnlyProp r) where
    encodeJson (OnlyProp name value)
        = CA.encode propCodec { name, value }



foreign import execute_ :: forall m e. Blessed m e -> Effect Unit