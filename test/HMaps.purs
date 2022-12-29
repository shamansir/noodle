module Test.HMaps where



import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding
    ( class HFoldlWithIndex
    , class FoldingWithIndex
    , hfoldlWithIndex
    )
import Heterogeneous.Mapping
    ( class HMapWithIndex
    , class Mapping
    , class MappingWithIndex
    , hmap, hmapWithIndex
    -- , class HFoldlWithIndex
    )
import Prim.Row (class Cons) as Row

import Record as Record



data AddOneAndShow = AddOneAndShow

instance addOneAndShow ::
  (Semiring n, Show n) =>
  Mapping AddOneAndShow n String where
  mapping AddOneAndShow = add one >>> show

--
-- fooAaa = hmap (show >>> Console.log) { c : 20, a : "foo" }
-- fooAaa = hmap (show) { c : 20, a : "foo" }
fooAaa :: { c :: String, a :: String, in :: String }
fooAaa = hmap (AddOneAndShow) { c : 20, a : 3, in : { a : 12 } }



newtype ZipProps fns = ZipProps { | fns }


instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProps fns) (SProxy sym) a b where
  mappingWithIndex (ZipProps fns) prop = Record.get prop fns


{- test :: { a :: Int, b :: Tuple String Number, c :: Boolean }
test =
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
        } -}

-- { a: 13, b: (Tuple "bar" 42.0), c: false }


data ShowProps = ShowProps


instance showProps ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowProps (SProxy sym) String a String where
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


-- test :: String
test = showRecord { a: "foo" , b: 42 , c: false }


{-
showWithIndex :: forall hlist.
  HFoldlWithIndex ShowWithIndex (Array (Tuple Int String)) hlist (Array (Tuple Int String)) =>
  hlist ->
  Array (Tuple Int String)
showWithIndex =
  hfoldlWithIndex ShowWithIndex ([] :: Array (Tuple Int String))
  -}