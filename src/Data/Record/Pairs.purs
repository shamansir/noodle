module Data.Record.Pairs where

import Prelude

import Data.Symbol (class IsSymbol)


import Prim.RowList as RL

class Pairs (xsA :: RL.RowList Type) (xsB :: RL.RowList Type)


class Converts a b where
    convert :: a -> b


instance pairsNilNil :: Pairs RL.Nil RL.Nil
else instance pairsConsNil ::
  ( IsSymbol nameA
  , Pairs tailA RL.Nil
  ) => Pairs (RL.Cons nameA tA tailA) RL.Nil
else instance pairsNilCons ::
  ( IsSymbol nameB
  , Pairs RL.Nil tailB
  ) => Pairs RL.Nil (RL.Cons nameB tB tailB)
else instance pairsConsCons ::
  ( IsSymbol nameA
  , IsSymbol nameB
  , Pairs (RL.Cons nameA tA tailA) tailB
  , Pairs tailA (RL.Cons nameB tB tailB)
  , Pairs tailA tailB
  , Converts tA tB
  ) => Pairs (RL.Cons nameA tA tailA) (RL.Cons nameB tB tailB)


{-
testPairs :: forall ra rlA rb rlB. Pairs rlA rlB => RL.RowToList ra rlA => RL.RowToList rb rlB => Record ra -> Record rb -> Unit
testPairs _ _ = unit


testPairs2 :: forall ra rlA rb rlB. Pairs rlA rlB => Pairs rlB rlA => RL.RowToList ra rlA => RL.RowToList rb rlB => Record ra -> Record rb -> Unit
testPairs2 _ _ = unit


foo1 :: Unit
foo1 = testPairs { a : "foo", b : 1 } { a : true, b : "x", c : 2.0 }


foo2 :: Unit
foo2 = testPairs { a : "foo", b : 1 } { foo : true, bar : "x" }


foo3 :: Unit
foo3 = testPairs2 { a : "foo", b : 1 } { a : true, b : "x", c : 2.0 }


foo4 :: Unit
foo4 = testPairs2 { a : "foo", b : 1 } { foo : true, bar : "x" }


instance Converts Int String where
    convert _ = "42"


instance Converts Int Number where
    convert _ = 42.0


instance Converts Int Boolean where
    convert _ = false


instance Converts Number Int where
    convert _ = 42


instance Converts Number String where
    convert _ = "42"


instance Converts String Number where
    convert _ = 42.0


instance Converts String Boolean where
    convert _ = false


instance Converts String String where
    convert = identity


instance Converts String Int where
    convert _ = 42


instance Converts Boolean Int where
    convert _ = 42


instance Converts Boolean String where
    convert _ = "42"
-}