module Test.HMaps where



import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding
    ( class HFoldlWithIndex
    , class FoldingWithIndex
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
import Type.Proxy (Proxy)

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


data ToArray = ToArray

instance mappingToArray ::
  Mapping ToArray a (Array (Tuple x b)) where
  mapping ToArray = const []


instance ixMappingToArray ::
  MappingWithIndex ToArray i a (Array (Tuple x b)) where
  mappingWithIndex ToArray _ = const []


fooBbb :: forall t194 t195 t202 t203 t210 t211.
      { a :: Array (Tuple t195 t194)
      , c :: Array (Tuple t203 t202)
      , in :: Array (Tuple t211 t210)
      }
fooBbb = hmap (ToArray) { c : 20, a : 3, in : { a : 12 } }


data ToArrayAToX x = ToArrayAToX

data ToArrayToX x = ToArrayToX


class AToX a x where
  aToX :: a -> x


class ToX x where
  toX :: forall a. a -> x


instance AToX Int RecFoo where
  aToX :: Int -> RecFoo
  aToX n = RecFoo { foo : n + 2 }


{-}
instance Semigroup a => ToX (RecSG a) where
  --toX :: forall a'. a' -> RecSG a
  toX n = RecSG { sg : n <> n } -}


instance AToX String RecFoo where
  aToX :: String -> RecFoo
  aToX str = RecFoo { foo : String.length str + 2 }


instance AToX Int RecBar where
  aToX :: Int -> RecBar
  aToX n = RecBar { bar : show (n - 2) }


instance AToX String RecBar where
  aToX :: String -> RecBar
  aToX str = RecBar { bar : str <> show (String.length str) }


instance mappingToArrayAToX ::
  AToX a x =>
  Mapping (ToArrayAToX x) a (Array (x /\ a)) where
  mapping ToArrayAToX a = [ aToX a /\ a ]


instance ixMappingToArrayAToX ::
  AToX a x =>
  MappingWithIndex (ToArrayAToX x) i a (Array (i /\ x /\ a)) where
  mappingWithIndex ToArrayAToX idx a = [ idx /\ aToX a /\ a ]


instance mappingToArrayToX ::
  ToX x =>
  Mapping (ToArrayToX x) a (Array (x /\ a)) where
  mapping ToArrayToX a = [ toX a /\ a ]


instance ixMappingToArrayToX ::
  ToX x =>
  MappingWithIndex (ToArrayToX x) i a (Array (i /\ x /\ a)) where
  mappingWithIndex ToArrayToX idx a = [ idx /\ toX a /\ a ]


newtype RecFoo = RecFoo { foo :: Int }
newtype RecBar = RecBar { bar :: String }

newtype RecSG a = RecSG { sg :: a }

derive newtype instance Eq RecFoo
derive newtype instance Eq RecBar


testWithAToXFoo =
  hmap (ToArrayAToX :: ToArrayAToX RecFoo) { c : 20, a : 3, f : 15, x : "aaa" }
  ==
    { c : [ RecFoo { foo : 22 } /\ 20 ]
    , a : [ RecFoo { foo : 5 } /\ 3 ]
    , f : [ RecFoo { foo : 17 } /\ 15 ]
    , x : [ RecFoo { foo : 5 } /\ "aaa" ]
    -- , d : [ Rec { foo : 15 } /\ "aaa" ]
    }


testWithAToXBar =
  hmap (ToArrayAToX :: ToArrayAToX RecBar) { c : 20, a : 3, f : 15, x : "aaa" }
  ==
    { c : [ RecBar { bar : "18" } /\ 20 ]
    , a : [ RecBar { bar : "1" } /\ 3 ]
    , f : [ RecBar { bar : "13" } /\ 15 ]
    , x : [ RecBar { bar : "aaa3" } /\ "aaa" ]
    -- , d : [ Rec { foo : 15 } /\ "aaa" ]
    }


{-
testWithToXFoo =
  hmap (ToArrayToX :: ToArrayToX RecFoo) { c : 20, a : 3, f : 15, x : "aaa" }
  ==
    { c : [ RecFoo { foo : 22 } /\ 20 ]
    , a : [ RecFoo { foo : 5 } /\ 3 ]
    , f : [ RecFoo { foo : 17 } /\ 15 ]
    , x : [ RecFoo { foo : 5 } /\ "aaa" ]
    -- , d : [ Rec { foo : 15 } /\ "aaa" ]
    }


testWithToXBar =
  hmap (ToArrayToX :: ToArrayToX RecBar) { c : 20, a : 3, f : 15, x : "aaa" }
  ==
    { c : [ RecBar { bar : "18" } /\ 20 ]
    , a : [ RecBar { bar : "1" } /\ 3 ]
    , f : [ RecBar { bar : "13" } /\ 15 ]
    , x : [ RecBar { bar : "aaa3" } /\ "aaa" ]
    -- , d : [ Rec { foo : 15 } /\ "aaa" ]
    }
-}


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


type TestRows = ( a :: String, b :: Int, c :: Boolean )


-- test :: String
test = showRecord { a: "foo" , b: 42 , c: false }


data ShowValues = ShowValues

instance showValues ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowValues (Proxy sym) String a String
  where
  foldingWithIndex _ _ str a = pre <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "

showTwice :: forall r.
  HFoldlWithIndex ShowProps String { | r } String =>
  HFoldlWithIndex ShowValues String { | r } String =>
  { | r } ->
  String
showTwice r = do
  let a = "{ " <> hfoldlWithIndex ShowProps "" r <> " }"
      b = "[ " <> hfoldlWithIndex ShowValues "" r <> " ]"
  a <> b



test' = showTwice { a: "foo" , b: 42 , c: false }

{-
showWithIndex :: forall hlist.
  HFoldlWithIndex ShowWithIndex (Array (Tuple Int String)) hlist (Array (Tuple Int String)) =>
  hlist ->
  Array (Tuple Int String)
showWithIndex =
  hfoldlWithIndex ShowWithIndex ([] :: Array (Tuple Int String))
