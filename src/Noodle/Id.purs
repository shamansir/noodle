module Noodle.Id
    ( Family(..), Family', FamilyR
    , family', familyR, familyP
    , reflectFamily, reflectFamily', reflectFamily'', reflectFamilyR
    , keysToFamiliesR
    , Input(..), Input', InputR
    , input', inputR, inputR', inputP
    , reflectInput, reflectInput', reflectInputR
    , keysToInputsR
    , Output(..), Output', OutputR
    , output', outputR, outputR', outputP
    , reflectOutput, reflectOutput', reflectOutputR
    , keysToOutputsR
    , NodeId, NodeId', NodeIdR
    , makeNodeId
    , split, split', splitR
    , nodeId', nodeIdR, nodeIdR'
    , reflectNodeId, reflectNodeId', reflectNodeIdR
    , familyOf, hashOf
    , class Reflect, reflect
    , class Reflect', reflect'
    , class FromKeysR, fromKeysR
    -- FIXME: make classes below internal
    , class HasInputsAt, class HasOutputsAt
    , class HasInput, class HasOutput, class HasFamily
    , class HasInputs, inputs, class HasOutputs, outputs
    , class ListsFamilies, class ListsInstances
    -- , class HasInputs', inputs'
    -- , class IsSymbol
    )
    where

import Prelude


import Effect (Effect)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UniqueHash (UniqueHash)
import Data.UniqueHash as UniqueHash
import Data.List (List)
import Data.SOrder (SOrder)
import Data.SOrder as SOrder

import Record.Extra as Record
import Prim.Row as R
import Type.RowList as RL


-- naming in general as follows:
-- SomeId (s :: Symbol) = SomeId -- contructor is open
-- SomeId' (s :: Symbol) = SomeId' String  -- contructor is closed, stores symbol and representation
-- SomeIdR = SomeIdR String  -- contructor is closed, R is for "raw", stores only representation


class Reflect (proxy :: Symbol -> Type) where -- almost the same as IsSymbol
    reflect :: forall sym. IsSymbol sym => proxy sym -> String


class Reflect' a where
    reflect' :: a -> String


class FromKeysR a where
    fromKeysR :: forall (w :: Row Type -> Type) (rows :: Row Type) rl. RL.RowToList rows rl => Record.Keys rl => SOrder -> w rows -> List a


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
instance FromKeysR FamilyR where fromKeysR = keysToFamiliesR


family' :: forall f. IsSymbol f => Family f -> Family' f
family' = reflectSymbol >>> Family'


familyR :: forall f. IsSymbol f => Family f -> FamilyR
familyR = reflectSymbol >>> FamilyR


reflectFamily :: forall f. IsSymbol f => Family f -> String
reflectFamily = reflectSymbol


reflectFamily' :: forall f. Family' f -> String
reflectFamily' (Family' s) = s


reflectFamily'' :: forall f. IsSymbol f => Family' f -> FamilyR
reflectFamily'' = reflectSymbol >>> FamilyR


reflectFamilyR :: FamilyR -> String
reflectFamilyR (FamilyR s) = s


familyP :: forall proxy f. IsSymbol f => proxy f -> Family' f
familyP = reflectSymbol >>> Family'


keysToFamiliesR :: forall w fs rl. RL.RowToList fs rl => Record.Keys rl => SOrder -> w fs -> List FamilyR
keysToFamiliesR order = Record.keys >>> SOrder.sortL' order >>> (<$>) FamilyR


data Input (i :: Symbol) = Input -- TODO: Int
instance Reflect Input where reflect = reflectInput


newtype Input' (i :: Symbol) = Input' String -- TODO: Int /\ String
derive newtype instance eqInput' :: Eq (Input' i)
derive newtype instance ordInput' :: Ord (Input' i)
derive newtype instance showInput' :: Show (Input' i)
instance Reflect' (Input' i) where reflect' = reflectInput'


newtype InputR = InputR String -- TODO: Int /\ String
derive newtype instance eqInputR :: Eq InputR
derive newtype instance ordInputR :: Ord InputR
derive newtype instance showInputR :: Show InputR
instance Reflect' InputR where reflect' = reflectInputR
instance FromKeysR InputR where fromKeysR = keysToInputsR


input' :: forall i. IsSymbol i => Input i -> Input' i
input' = inputP


inputR :: forall i. IsSymbol i => Input i -> InputR
inputR = reflectSymbol >>> InputR

inputR' :: forall i. Input' i -> InputR
inputR' = reflectInput' >>> InputR


inputP :: forall proxy i. IsSymbol i => proxy i -> Input' i
inputP = reflectSymbol >>> Input'


reflectInput :: forall i. IsSymbol i => Input i -> String
reflectInput = reflectSymbol


reflectInput' :: forall i. Input' i -> String
reflectInput' (Input' s) = s


reflectInputR :: InputR -> String
reflectInputR (InputR s) = s


keysToInputsR :: forall w is rl. HasInputsAt is rl => SOrder -> w is -> List InputR
keysToInputsR order = Record.keys >>> SOrder.sortL' order >>> (<$>) InputR


-- _in :: InputR -> String
-- _in = reflect'


data Output (o :: Symbol) = Output -- TODO: Int
instance Reflect Output where reflect = reflectOutput


newtype Output' (o :: Symbol) = Output' String -- TODO: Int /\ String
derive newtype instance eqOutput' :: Eq (Output' o)
derive newtype instance ordOutput' :: Ord (Output' o)
derive newtype instance showOutput' :: Show (Output' o)
instance Reflect' (Output' o) where reflect' = reflectOutput'


newtype OutputR = OutputR String -- TODO: Int /\ String
derive newtype instance eqOutputR :: Eq OutputR
derive newtype instance ordOutputR :: Ord OutputR
derive newtype instance showOutputR :: Show OutputR
instance Reflect' OutputR where reflect' = reflectOutputR
instance FromKeysR OutputR where fromKeysR = keysToOutputsR


output' :: forall o. IsSymbol o => Output o -> Output' o
output' = reflectSymbol >>> Output'


outputR :: forall o. IsSymbol o => Output o -> OutputR
outputR = reflectSymbol >>> OutputR


outputR' :: forall o. Output' o -> OutputR
outputR' = reflectOutput' >>> OutputR


outputP :: forall proxy o. IsSymbol o => proxy o -> Output' o
outputP = reflectSymbol >>> Output'


reflectOutput :: forall o. IsSymbol o => Output o -> String
reflectOutput = reflectSymbol


reflectOutput' :: forall o. Output' o -> String
reflectOutput' (Output' s) = s


reflectOutputR :: OutputR -> String
reflectOutputR (OutputR s) = s


keysToOutputsR :: forall w os rl. HasInputsAt os rl => SOrder -> w os -> List OutputR -- TODO: Array OutputR?
keysToOutputsR order = Record.keys >>> SOrder.sortL' order >>> (<$>) OutputR


-- _in :: InputR -> String
-- _in = reflect'


newtype NodeId f = NodeId (Family' f /\ UniqueHash)
derive newtype instance eqNodeId :: Eq (NodeId f)
derive newtype instance ordNodeId :: Ord (NodeId f)
derive newtype instance showNodeId :: Show (NodeId f)
instance Reflect' (NodeId f) where reflect' = joinDots <<< reflectNodeId


newtype NodeId' (f :: Symbol) = NodeId' (FamilyR /\ UniqueHash)
derive newtype instance eqNodeId' :: Eq (NodeId' f)
derive newtype instance ordNodeId' :: Ord (NodeId' f)
derive newtype instance showNodeId' :: Show (NodeId' f)
instance Reflect' (NodeId' o) where reflect' = joinDots <<< reflectNodeId'


newtype NodeIdR = NodeIdR (FamilyR /\ UniqueHash)
derive newtype instance eqNodeIdR :: Eq NodeIdR
derive newtype instance ordNodeIdR :: Ord NodeIdR
derive newtype instance showNodeIdR :: Show NodeIdR
instance Reflect' NodeIdR where reflect' = joinDots <<< reflectNodeIdR
-- instance FromKeysR NodeIdR where fromKeysR = keysToNodeIdsR


joinDots :: String /\ String -> String
joinDots = uncurry \a b -> a <> "::" <> b


makeNodeId :: forall f. Family' f -> Effect (NodeId f)
makeNodeId f = NodeId <$> ((/\) f) <$> UniqueHash.generate


familyOf :: forall f. NodeId f -> Family' f
familyOf (NodeId (family' /\ _)) = family'


hashOf :: forall f. NodeId f -> UniqueHash
hashOf (NodeId (_ /\ uuid)) = uuid


split :: forall f. IsSymbol f => NodeId f -> FamilyR /\ UniqueHash
split (NodeId (family' /\ uuid)) = reflectFamily'' family' /\ uuid


split' :: forall f. NodeId' f -> FamilyR /\ UniqueHash
split' (NodeId' pair) = pair


splitR :: NodeIdR -> FamilyR /\ UniqueHash
splitR (NodeIdR pair) = pair


nodeId' :: forall f. IsSymbol f => NodeId f -> NodeId' f
nodeId' (NodeId (family' /\ uuid)) = NodeId' $ reflectFamily'' family' /\ uuid


nodeIdR :: forall f. IsSymbol f => NodeId f -> NodeIdR
nodeIdR = split >>> NodeIdR


nodeIdR' :: forall f. NodeId' f -> NodeIdR
nodeIdR' = split' >>> NodeIdR


reflectNodeId :: forall f. NodeId f -> String /\ String
reflectNodeId (NodeId (family' /\ uuid)) = reflect' family' /\ UniqueHash.toString uuid


reflectNodeId' :: forall f. NodeId' f -> String /\ String
reflectNodeId' (NodeId' (familyR /\ uuid)) = reflect' familyR /\ UniqueHash.toString uuid


reflectNodeIdR :: NodeIdR -> String /\ String
reflectNodeIdR (NodeIdR (familyR /\ uuid)) = reflect' familyR /\ UniqueHash.toString uuid


-- TODO: extend to HasInputs, HasOutputs with getAtInput, getAtOutput, updateInputs, updateOutputs, ...
 -- FIXME: use newtype
-- FIXME: another module?
class (RL.RowToList is rli, Record.Keys rli) <= HasInputsAt is rli
instance (RL.RowToList is rli, Record.Keys rli) => HasInputsAt is rli


class HasInputsAt is rli <= HasInputs is rli a | a -> is, a -> rli
    where inputs :: a -> List InputR
-- class HasInputsAt is rli <= HasInputs' is rli
--     where
--         --inputs' :: forall proxy. proxy is -> List InputR
--         inputs' :: Unit -> List InputR
class HasOutputsAt os rlo <= HasOutputs os rlo a | a -> os, a -> rlo
    where outputs :: a -> List OutputR

class (RL.RowToList os rlo, Record.Keys rlo) <= HasOutputsAt os rlo
instance (RL.RowToList os rlo, Record.Keys rlo) => HasOutputsAt os rlo


class (RL.RowToList fs rlf, Record.Keys rlf) <= ListsFamilies fs rlf
instance (RL.RowToList fs rlf, Record.Keys rlf) => ListsFamilies fs rlf


class (RL.RowToList ins rlin, Record.Keys rlin) <= ListsInstances ins rlin
instance (RL.RowToList ins rlin, Record.Keys rlin) => ListsInstances ins rlin


-- class HasInput :: forall k. Symbol -> k -> Row k -> Row k -> Constraint
class (IsSymbol i, R.Cons i din is' is) <= HasInput i din is' is
instance (IsSymbol i, R.Cons i din is' is) => HasInput i din is' is
class (IsSymbol o, R.Cons o dout os' os) <= HasOutput o dout os' os
instance (IsSymbol o, R.Cons o dout os' os) => HasOutput o dout os' os

class (IsSymbol f, R.Cons f x fs' fs) <= HasFamily f x fs' fs
instance (IsSymbol f, R.Cons f x fs' fs) => HasFamily f x fs' fs

{-
class IsSymbol f <= IsSymbol f
instance IsSymbol f => IsSymbol f
-}