module Noodle.Id
    ( Family(..), Family', FamilyR
    , family', familyR, familyP
    , reflectFamily, reflectFamily', reflectFamily'', reflectFamilyR
    , keysToFamiliesR
    , Input(..), Input', InputR
    , input', inputR, inputR', inputP, inputP'
    , reflectInput, reflectInput', reflectInputR
    , keysToInputsR
    , Output(..), Output', OutputR
    , output', outputR, outputR', outputP, outputP'
    , reflectOutput, reflectOutput', reflectOutputR
    , keysToOutputsR
    , NodeId, NodeId', NodeIdR
    , makeNodeId
    , split, split', splitR
    , nodeId', nodeIdR, nodeIdR'
    , reflectNodeId, reflectNodeId', reflectNodeIdR
    , familyOf, hashOf, hashOfR
    , class Reflect, reflect
    , class Reflect', reflect'
    , class Indexed, index
    , class FromKeysR, fromKeysR
    -- FIXME: make classes below internal
    , class HasInputsAt, class HasOutputsAt
    , class HasInput, class HasOutput, class HasFamily
    , class HasInputs, inputs, class HasOutputs, outputs
    , class ListsFamilies, class ListsInstances
    , HoldsFamily, holdFamily, withFamily
    , HoldsNodeId, holdNodeId, withNodeId
    -- , class HasInputs', inputs'
    -- , class IsSymbol
    )
    where

import Prelude

import Color.Scheme.X11 (wheat)
import Data.List (List)
import Data.List (mapWithIndex) as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.SOrder (SOrder)
import Data.SOrder as SOrder
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple (uncurry, curry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UniqueHash (UniqueHash)
import Data.UniqueHash as UniqueHash
import Effect (Effect)
import Prim.Row as R
import Record.Extra as Record
import Type.Proxy (Proxy(..))
import Type.RowList as RL


-- naming in general as follows:
-- SomeId (s :: Symbol) = SomeId -- contructor is open
-- SomeId' (s :: Symbol) = SomeId' String  -- contructor is closed, stores symbol and representation
-- SomeIdR = SomeIdR String  -- contructor is closed, R is for "raw", stores only representation


class Reflect (proxy :: Symbol -> Type) where -- almost the same as IsSymbol
    reflect :: forall sym. IsSymbol sym => proxy sym -> String


class Reflect' a where
    reflect' :: a -> String


class Indexed a where
    index :: a -> Int


class FromKeysR a where
    fromKeysR :: forall (w :: Row Type -> Type) (rows :: Row Type) rl. RL.RowToList rows rl => Record.Keys rl => SOrder -> w rows -> List a


instance Reflect proxy where
    reflect = reflectSymbol


toPair :: forall proxy sym. IsSymbol sym => Reflect proxy => Indexed (proxy sym) => proxy sym -> Int /\ String
toPair p =  index p /\ reflect p


toPair' :: forall a. Reflect' a => Indexed a => a -> Int /\ String
toPair' p = index p /\ reflect' p


data Family (f :: Symbol) = Family
-- instance Reflect Family where reflect = reflectFamily -- covered by Reflect proxy instance


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
family' = reflect >>> Family'


familyR :: forall f. IsSymbol f => Family f -> FamilyR
familyR = reflect >>> FamilyR


reflectFamily :: forall f. IsSymbol f => Family f -> String
reflectFamily = reflect


reflectFamily' :: forall f. Family' f -> String
reflectFamily' (Family' s) = s


reflectFamily'' :: forall f. IsSymbol f => Family' f -> FamilyR
reflectFamily'' = reflect >>> FamilyR


reflectFamilyR :: FamilyR -> String
reflectFamilyR (FamilyR s) = s


familyP :: forall proxy f. IsSymbol f => proxy f -> Family' f
familyP = reflect >>> Family'


keysToFamiliesR :: forall w fs rl. RL.RowToList fs rl => Record.Keys rl => SOrder -> w fs -> List FamilyR
keysToFamiliesR order = Record.keys >>> SOrder.sortL' order >>> (<$>) FamilyR


newtype HoldsFamily = HoldsFamily (forall r. (forall sym. IsSymbol sym => Family sym -> r) -> r)


holdFamily :: forall sym. IsSymbol sym => Family sym -> HoldsFamily
holdFamily sym = HoldsFamily (_ $ sym)


withFamily :: forall r. HoldsFamily -> (forall sym. IsSymbol sym => Family sym -> r) -> r
withFamily (HoldsFamily fn) = fn


-- test1 :: HoldsFamily
-- test1 = holdFamily (Family :: Family "foo")


-- test2 :: String
-- test2 = withFamily test1 reflectFamily


newtype HoldsNodeId = HoldsNodeId (forall r. (forall sym. IsSymbol sym => NodeId sym -> r) -> r)


holdNodeId :: forall sym. IsSymbol sym => NodeId sym -> HoldsNodeId
holdNodeId sym = HoldsNodeId (_ $ sym)


withNodeId :: forall r. HoldsNodeId -> (forall sym. IsSymbol sym => NodeId sym -> r) -> r
withNodeId (HoldsNodeId fn) = fn


--test3 :: forall sym. Family "foo"
-- test3 = withFamily test1 identity


data Input (i :: Symbol) = Input Int -- TODO: Int
-- instance Reflect Input where reflect = reflectInput -- covered by Reflect proxy instance
instance Indexed (Input i) where index (Input iindex) = iindex


newtype Input' (i :: Symbol) = Input' (Int /\ String) -- TODO: Int /\ String
derive newtype instance eqInput' :: Eq (Input' i)
instance ordInput' :: Ord (Input' i) where compare (Input' a) (Input' b) = compare (Tuple.fst a) (Tuple.fst b)
derive newtype instance showInput' :: Show (Input' i)
instance Reflect' (Input' i) where reflect' = reflectInput'
instance Indexed (Input' i) where index (Input' pair) = Tuple.fst pair


newtype InputR = InputR (Int /\ String) -- TODO: Int /\ String
derive newtype instance eqInputR :: Eq InputR
instance ordInputR :: Ord InputR where compare (InputR a) (InputR b) = compare (Tuple.fst a) (Tuple.fst b)
derive newtype instance showInputR :: Show InputR
instance Reflect' InputR where reflect' = reflectInputR
instance Indexed InputR where index (InputR pair) = Tuple.fst pair
instance FromKeysR InputR where fromKeysR = keysToInputsR


input' :: forall i. IsSymbol i => Input i -> Input' i
input' (Input n) = Input' $ n /\ reflect (Proxy :: _ i)


inputR :: forall i. IsSymbol i => Input i -> InputR
inputR = toPair >>> InputR


inputR' :: forall i. Input' i -> InputR
inputR' = toPair' >>> InputR


inputP :: forall proxy i. IsSymbol i => Indexed (proxy i) => Reflect proxy => proxy i -> Input' i
inputP = toPair >>> Input'


inputP' :: forall proxy i. IsSymbol i => Reflect proxy => SOrder -> proxy i -> Input' i
inputP' order p = Input' $ fromMaybe (-1) (SOrder.index' order $ reflect p) /\ reflect p


reflectInput :: forall i. IsSymbol i => Input i -> String
reflectInput = reflect


reflectInput' :: forall i. Input' i -> String
reflectInput' (Input' pair) = Tuple.snd pair


reflectInputR :: InputR -> String
reflectInputR (InputR pair) = Tuple.snd pair


keysToInputsR :: forall w is rl. HasInputsAt is rl => SOrder -> w is -> List InputR
keysToInputsR order = Record.keys >>> SOrder.sortL' order >>> List.mapWithIndex (/\) >>> map InputR


-- _in :: InputR -> String
-- _in = reflect'


data Output (o :: Symbol) = Output Int
-- instance Reflect Output where reflect = reflectOutput -- covered by Reflect proxy instance
instance Indexed (Output o) where index (Output oindex) = oindex


newtype Output' (o :: Symbol) = Output' (Int /\ String) -- TODO: Int /\ String
derive newtype instance eqOutput' :: Eq (Output' o)
instance ordOutput' :: Ord (Output' i) where compare (Output' a) (Output' b) = compare (Tuple.fst a) (Tuple.fst b)
derive newtype instance showOutput' :: Show (Output' o)
instance Reflect' (Output' o) where reflect' = reflectOutput'
instance Indexed (Output' i) where index (Output' pair) = Tuple.fst pair


newtype OutputR = OutputR (Int /\ String) -- TODO: Int /\ String
derive newtype instance eqOutputR :: Eq OutputR
instance ordOutputR :: Ord OutputR where compare (OutputR a) (OutputR b) = compare (Tuple.fst a) (Tuple.fst b)
derive newtype instance showOutputR :: Show OutputR
instance Reflect' OutputR where reflect' = reflectOutputR
instance Indexed OutputR where index (OutputR pair) = Tuple.fst pair
instance FromKeysR OutputR where fromKeysR = keysToOutputsR


output' :: forall o. IsSymbol o => Output o -> Output' o
output' = toPair >>> Output'


outputR :: forall o. IsSymbol o => Output o -> OutputR
outputR = toPair >>> OutputR


outputR' :: forall o. Output' o -> OutputR
outputR' = toPair' >>> OutputR


outputP :: forall proxy o. IsSymbol o => Indexed (proxy o) => proxy o -> Output' o
outputP = toPair >>> Output'


outputP' :: forall proxy o. IsSymbol o => Reflect proxy => SOrder -> proxy o -> Output' o
outputP' order p = Output' $ fromMaybe (-1) (SOrder.index' order $ reflect p) /\ reflect p


reflectOutput :: forall o. IsSymbol o => Output o -> String
reflectOutput = reflect


reflectOutput' :: forall o. Output' o -> String
reflectOutput' (Output' s) = Tuple.snd s


reflectOutputR :: OutputR -> String
reflectOutputR (OutputR s) = Tuple.snd s


keysToOutputsR :: forall w os rl. HasInputsAt os rl => SOrder -> w os -> List OutputR -- TODO: Array OutputR?
keysToOutputsR order = Record.keys >>> SOrder.sortL' order >>> List.mapWithIndex (/\) >>> map OutputR


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


hashOfR :: forall f. NodeIdR -> UniqueHash
hashOfR (NodeIdR (_ /\ uuid)) = uuid


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