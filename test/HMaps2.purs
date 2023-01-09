module Test.HMaps2 where

import Effect (Effect)
import Effect.Console (log)

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
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
    ( class HMapWithIndex
    , class Mapping
    , class MappingWithIndex
    , hmap, hmapWithIndex
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

data ToFlowLists src = ToFlowLists ( Record src )


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
        (ToFlowLists rl)
        (Proxy sym)
        (state /\ Record is /\ Record os)
        (String /\ List String /\ List String)
    where
    mappingWithIndex (ToFlowLists rec) prop (s /\ iRec /\ oRec) =
        show s /\ Record.keys (Proxy :: Proxy is) /\ Record.keys (Proxy :: Proxy os)



testToFlow rec = hmapWithIndex (ToFlowLists rec) rec


testToFlowImpl = testToFlow someRec


data MyRepr
    = Unit_
    | String_ String
    | Int_ Int
    | Bool_ Boolean
    | Other_


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