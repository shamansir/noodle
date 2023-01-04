module Noodle.Id
    ( Family(..), Family', FamilyR
    , family', familyR
    , reflectFamily, reflectFamily', reflectFamilyR
    , Input(..), Input', InputR
    , input', inputR
    , reflectInput, reflectInput', reflectInputR
    , Output(..), Output', OutputR
    , output', outputR
    , reflectOutput, reflectOutput', reflectOutputR
    , NodeId, makeNodeId, reflectNodeId
    , class Reflect, reflect
    , class Reflect', reflect'
    )
    where

import Prelude


import Effect (Effect)
import Data.Symbol
import Unsafe.Coerce (unsafeCoerce)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UUID (UUID)
import Data.UUID as UUID

import Type.Proxy (Proxy)


-- naming in general as follows:
-- SomeId (s :: Symbol) = SomeId -- contructor is open
-- SomeId' (s :: Symbol) = SomeId' String  -- contructor is closed, stores symbol and representation
-- SomeIdR = SomeIdR String  -- contructor is closed, R is for "raw", stores only representation


class Reflect' a where
    reflect' :: a -> String

class Reflect (proxy :: Symbol -> Type) where -- almost the same as IsSymbol
    reflect :: forall sym. IsSymbol sym => proxy sym -> String


data Family (f :: Symbol) = Family
instance Reflect Family where reflect = reflectFamily


newtype Family' (f :: Symbol) = Family' String
derive newtype instance eqFamily' :: Eq (Family' f)
derive newtype instance ordFamily' :: Ord (Family' f)
derive newtype instance showFamily' :: Show (Family' f)
instance Reflect' (Family' f) where reflect' = reflectFamily'


newtype FamilyR = FamilyR String
derive newtype instance eqFamilyR :: Eq FamilyR
derive newtype instance ordFamilyR :: Ord FamilyR
derive newtype instance showFamilyR :: Show FamilyR
instance Reflect' FamilyR where reflect' = reflectFamilyR


family' :: forall f. IsSymbol f => Family f -> Family' f
family' = reflectSymbol >>> Family'


familyR :: forall f. IsSymbol f => Family f -> FamilyR
familyR = reflectSymbol >>> FamilyR


reflectFamily :: forall f. IsSymbol f => Family f -> String
reflectFamily = reflectSymbol


reflectFamily' :: forall f. Family' f -> String
reflectFamily' (Family' s) = s


reflectFamilyR :: FamilyR -> String
reflectFamilyR (FamilyR s) = s


data Input (i :: Symbol) = Input
instance Reflect Input where reflect = reflectInput


newtype Input' (i :: Symbol) = Input' String
derive newtype instance eqInput' :: Eq (Input' i)
derive newtype instance ordInput' :: Ord (Input' i)
derive newtype instance showInput' :: Show (Input' i)
instance Reflect' (Input' i) where reflect' = reflectInput'


newtype InputR = InputR String
derive newtype instance eqInputR :: Eq InputR
derive newtype instance ordInputR :: Ord InputR
derive newtype instance showInputR :: Show InputR
instance Reflect' InputR where reflect' = reflectInputR


input' :: forall i. IsSymbol i => Input i -> Input' i
input' = reflectSymbol >>> Input'


inputR :: forall i. IsSymbol i => Input i -> InputR
inputR = reflectSymbol >>> InputR


reflectInput :: forall i. IsSymbol i => Input i -> String
reflectInput = reflectSymbol


reflectInput' :: forall i. Input' i -> String
reflectInput' (Input' s) = s


reflectInputR :: InputR -> String
reflectInputR (InputR s) = s


data Output (o :: Symbol) = Output
instance Reflect Output where reflect = reflectOutput


newtype Output' (o :: Symbol) = Output' String
derive newtype instance eqOutput' :: Eq (Output' o)
derive newtype instance ordOutput' :: Ord (Output' o)
derive newtype instance showOutput' :: Show (Output' o)
instance Reflect' (Output' o) where reflect' = reflectOutput'


newtype OutputR = OutputR String
derive newtype instance eqOutputR :: Eq OutputR
derive newtype instance ordOutputR :: Ord OutputR
derive newtype instance showOutputR :: Show OutputR
instance Reflect' OutputR where reflect' = reflectOutputR


output' :: forall o. IsSymbol o => Output o -> Output' o
output' = reflectSymbol >>> Output'


outputR :: forall o. IsSymbol o => Output o -> OutputR
outputR = reflectSymbol >>> OutputR


reflectOutput :: forall o. IsSymbol o => Output o -> String
reflectOutput = reflectSymbol


reflectOutput' :: forall o. Output' o -> String
reflectOutput' (Output' s) = s


reflectOutputR :: OutputR -> String
reflectOutputR (OutputR s) = s


newtype NodeId f = NodeId (Family' f /\ UUID)
derive newtype instance eqNodeId :: Eq (NodeId f)
derive newtype instance ordNodeId :: Ord (NodeId f)
derive newtype instance showNodeId :: Show (NodeId f)
instance Reflect' (NodeId f) where reflect' = reflectNodeId


reflectNodeId :: forall f. NodeId f -> String
reflectNodeId (NodeId (family' /\ uuid)) = reflect' family' <> "::" <> UUID.toString uuid


makeNodeId :: forall f. Family' f -> Effect (NodeId f)
makeNodeId f = NodeId <$> ((/\) f) <$> UUID.generate