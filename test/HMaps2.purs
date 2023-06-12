module Test.HMaps2 where

import Effect (Effect)
import Effect.Console (log)

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List)
import Heterogeneous.Folding
    ( class HFoldlWithIndex
    , class FoldingWithIndex
    , hfoldl
    , hfoldlWithIndex
    , class FoldlRecord
    )
import Heterogeneous.Mapping
    ( class HMap
    , class HMapWithIndex
    , class MapRecordWithIndex
    , class Mapping
    , class MappingWithIndex
    , hmap, hmapWithIndex
    , ConstMapping
    -- , class HFoldlWithIndex
    )
import Prim.Row (class Cons) as Row
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Record as Record
import Record.Extra as Record

import Prelude



someRec =
        { foo :
            unit
            /\ { foo : "aaa", bar : "bbb", c : 32 }
            /\ { out : false }
        , bar :
            unit
            /\ { a : "aaa", b : "bbb", c : "ccc" }
            /\ { x : 12 }
        , sum :
            unit
            /\ { a : 40, b : 2 }
            /\ { sum : 42 }
        }

data ToFlowLists = ToFlowLists


expected1 =
    { foo :
        "unit" /\ [ "foo", "bar", "c" ] /\ [ "out " ]
    , bar :
        "unit" /\ [ "a", "b", "c" ] /\ [ "x" ]
    , sum :
        "unit" /\ [ "a", "b" ] /\ [ "sum" ]
    }

instance toFlowLists ::
    ( IsSymbol sym
    , Show state
    , RL.RowToList is iks, Record.Keys iks
    , RL.RowToList os oks, Record.Keys oks
    ) =>
    MappingWithIndex
        (ToFlowLists)
        (Proxy sym)
        (state /\ Record is /\ Record os)
        (String /\ List String /\ List String)
    where
    mappingWithIndex (ToFlowLists) prop (s /\ iRec /\ oRec) =
        show s /\ Record.keys (Proxy :: Proxy is) /\ Record.keys (Proxy :: Proxy os)



testToFlow = hmapWithIndex ToFlowLists


testToFlowImpl = testToFlow someRec



data ToReprTop repr = ToReprTop (Repr repr)
data ToReprDown repr = ToReprDown (Repr repr)


testToRepr repr = hmapWithIndex (ToReprTop repr)

testToReprImpl = testToRepr (Repr :: Repr MyRepr) someRec

{-
data Repr

foreign import data MakeRepr :: Type -> Type -> Repr
foreign import data EmptyRepr :: Repr

foreign import data FromRepr :: Repr -> Type -}


-- class HasRepr (repr :: Repr) a where
--     toRepr :: a -> FromRepr repr

data Repr :: forall k. k -> Type
data Repr a = Repr


class HasRepr repr a where
    toRepr :: a -> repr


instance toReprTopInstance ::
    ( HasRepr repr state
    -- , HMap ToReprDown (Proxy is) (Record repr_is)
    -- , HMap ToReprDown (Proxy os) (Record repr_os)
    , MapRecordWithIndex iks (ConstMapping (ToReprDown repr)) is repr_is
    , MapRecordWithIndex oks (ConstMapping (ToReprDown repr)) os repr_os
    , RL.RowToList is iks, Record.Keys iks
    , RL.RowToList os oks, Record.Keys oks
    ) =>
    MappingWithIndex
        (ToReprTop repr)
        (Proxy sym)
        (state /\ Record is /\ Record os)
        (repr /\ Record repr_is /\ Record repr_os)
        -- (FromRepr repr /\ Record repr_is /\ Record repr_os)
    where
    mappingWithIndex (ToReprTop repr) prop (s /\ iRec /\ oRec) =
        toRepr s
            /\ hmap (ToReprDown repr) iRec
            /\ hmap (ToReprDown repr) oRec


instance toReprDownInstance ::
    ( HasRepr repr a
    ) =>
    Mapping
        (ToReprDown repr)
        -- (Proxy sym)
        a
        repr -- (FromRepr repr)
    where
    mapping (ToReprDown _) a =
        toRepr a


testDownRepr ∷ ∀ (t311 ∷ Type) (t312 ∷ Type) repr. HMap (ToReprDown repr) t311 t312 ⇒ Repr repr -> t311 → t312
testDownRepr repr = hmap (ToReprDown repr)

testDownReprImpl = testDownRepr (Repr :: Repr MyRepr) { foo : "aaa", bar : "bbb", c : 32 }





data MyRepr
    = Unit_
    | String_ String
    | Int_ Int
    | Bool_ Boolean
    | Other_

instance Show MyRepr
    where
        show Unit_ = "Unit"
        show (String_ str) = "String::" <> str
        show (Int_ int) = "Int::" <> show int
        show (Bool_ bool) = "Bool_::" <> show bool
        show Other_ = "Other"


expected2 =
        { foo :
            Unit_
            /\ { foo : String_ "aaa", bar : String_ "bbb", c : Int_ 32 }
            /\ { out : Bool_ false }
        , bar :
            Unit_
            /\ { a : String_ "aaa", b : String_ "bbb", c : String_ "ccc" }
            /\ { x : Int_ 12 }
        , sum :
            Unit_
            /\ { a : Int_ 40, b : Int_ 2 }
            /\ { sum : Int_ 42 }
        }


instance HasRepr MyRepr String where toRepr = String_
instance HasRepr MyRepr Int where toRepr = Int_
instance HasRepr MyRepr Unit where toRepr _ = Unit_
instance HasRepr MyRepr Boolean where toRepr = Bool_










newtype ZipProps fns = ZipProps { | fns }


instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProps fns) (Proxy sym) a b where
  mappingWithIndex (ZipProps fns) prop = Record.get prop fns


testZip =
    let
        zipRecord = hmapWithIndex <<< ZipProps
    in
        { a: add 1
        , b: Tuple "bar"
        , c: \a -> not a
        }
        `zipRecord`
        { a: 12
        , b: 42.0
        , c: true
        }



data AddOneAndShow = AddOneAndShow

instance addOneAndShow ::
  (Semiring n, Show n) =>
  Mapping AddOneAndShow n String where
  mapping AddOneAndShow = add one >>> show


data AddOne = AddOne

instance addOne ::
  (Semiring n) =>
  Mapping AddOne n n where
  mapping AddOne = add one


testSimpleHMap1 = hmap AddOneAndShow { a: 1, b: 2.0, c: { x: 12, y: 42 } }


testSimpleHMap2 = hmap AddOne { a: 1, b: 2.0, c: { x: 12, y: 42 } }


testSimpleHFold1 :: Int
testSimpleHFold1 = hfoldl (add :: Int -> Int -> Int) 0 { a: 12, b: 42, c: 100 }



data ShowProps = ShowProps


instance showProps ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowProps (Proxy sym) String a String where
  foldingWithIndex ShowProps prop str a =
    pre <> reflectSymbol prop <> ": " <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "


showRecord :: forall r.
  HFoldlWithIndex ShowProps String { | r } String =>
  { | r } ->
  String
showRecord r =
  "{ " <> hfoldlWithIndex ShowProps "" r <> " }"


testSimpleHFold2 :: String
testSimpleHFold2 = showRecord { a: "foo" , b: 42 , c: false }



main :: Effect Unit
main = do
  log $ showRecord testToFlowImpl
  log $ showRecord testToReprImpl