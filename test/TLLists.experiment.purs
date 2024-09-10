module Test.TLLists.Experiment where

import Prelude


import Type.Data.List (List', Cons', Nil')


type TList = List'

type TCons = Cons'

type TNil = Nil'


type SList = TList Symbol


type MySymbols = (TCons "s" (TCons "d" TNil))