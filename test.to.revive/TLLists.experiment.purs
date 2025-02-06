module Test.TLLists.Experiment where

import Prelude

import Type.Proxy (Proxy(..))
import Type.Data.Boolean (False, True)
import Type.Data.List (List', Cons', Nil', type (:>))


import Type.Data.List.Extra (TList, TCons, TNil, class Put, class Merge, class Has)


type SList = TList Symbol


type MySymbols = ("s" :> ("d" :> TNil))


testPut :: forall a b. Put "x" a b => Proxy a -> Proxy b -> Unit
testPut _ _ = unit


testPutItem :: forall x a b. Put x a b => Proxy x -> Proxy a -> Proxy b -> Unit
testPutItem _ _ _ = unit


testPutNonEmpty :: Proxy ("x" :> "s" :> TNil) -> Unit
testPutNonEmpty = testPut (Proxy :: _ ("s" :> TNil))


testPutItemNonEmpty :: Proxy ("a" :> "b" :> TNil) -> Unit
testPutItemNonEmpty = testPutItem (Proxy :: _ "a") (Proxy :: _ ("b" :> TNil))


testPutItemSame :: Proxy ("a" :> "a" :> TNil) -> Unit -- FIXME: don't allow putting same items
testPutItemSame = testPutItem (Proxy :: _ "a") (Proxy :: _ ("a" :> TNil))


testPutEmpty :: Proxy ("x" :> TNil) -> Unit
testPutEmpty = testPut (Proxy :: _ TNil)


testPutItemEmpty :: Proxy ("a" :> "b" :> TNil) -> Unit
testPutItemEmpty = testPutItem (Proxy :: _ "a") (Proxy :: _ ("b" :> TNil))


testMerge :: forall a b c. Merge a b c => Proxy a -> Proxy b -> Proxy c -> Unit
testMerge _ _ _ = unit


testMergeEmpty :: Proxy TNil -> Unit
testMergeEmpty = testMerge (Proxy :: _ TNil) (Proxy :: _ TNil)


testMergeNonEmptyA :: Proxy ("a" :> TNil) -> Unit
testMergeNonEmptyA = testMerge (Proxy :: _ ("a" :> TNil)) (Proxy :: _ TNil)


testMergeNonEmptyB :: Proxy ("a" :> TNil) -> Unit
testMergeNonEmptyB = testMerge (Proxy :: _ TNil) (Proxy :: _ ("a" :> TNil))


testMergeNonEmptyB' :: Proxy ("a" :> "b" :> TNil) -> Unit
testMergeNonEmptyB' = testMerge (Proxy :: _ TNil) (Proxy :: _ ("a" :> "b" :> TNil))


testMergeNonEmptyAB :: Proxy ("a" :> "b" :> TNil) -> Unit
testMergeNonEmptyAB = testMerge (Proxy :: _ ("a" :> TNil)) (Proxy :: _ ("b" :> TNil))


testMergeNonEmptyAB' :: Proxy ("a" :> "c" :> "b" :> "d" :> TNil) -> Unit
testMergeNonEmptyAB' = testMerge (Proxy :: _ ("a" :> "c" :> TNil)) (Proxy :: _ ("b" :> "d" :> TNil))


testMergeNonEmptyAB'' :: Proxy ("a" :> "c" :> "b" :> "c" :> TNil) -> Unit
testMergeNonEmptyAB'' = testMerge (Proxy :: _ ("a" :> "c" :> TNil)) (Proxy :: _ ("b" :> "c" :> TNil))


testHas :: forall xs res. Has "a" xs res => Proxy res -> Proxy xs -> Unit
testHas _ _ = unit


testHasEmpty :: Unit
testHasEmpty = testHas (Proxy :: _ False) (Proxy :: _ TNil)


testHasNonEmpty :: Unit
testHasNonEmpty = testHas (Proxy :: _ True) (Proxy :: _ ("a" :> TNil))