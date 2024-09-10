module Test.MyToolkit.Toolkit where

import Prelude

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)

import Effect (Effect)

import Noodle.Toolkit (Toolkit) as Noodle
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F)


import Test.MyToolkit.Node.Sample as Sample
import Test.MyToolkit.Node.Sum as Sum
import Test.MyToolkit.Node.Concat as Concat
import Test.MyToolkit.Node.Stateful as Stateful
import Test.MyToolkit.Repr (ISRepr)


type MyFamilies :: Families
type MyFamilies
    =  Sample.F
    :> Sum.F
    :> Concat.F
    :> Stateful.F
    :> TNil


type Toolkit = Noodle.Toolkit MyFamilies ISRepr Effect


{-
toolkit :: Toolkit
toolkit =
    Toolkit.empty "my-toolkit"
        # Toolkit.register (Concat.family :: Concat.Family)
        # Toolkit.register (Sum.family Sum.sumBoth :: Sum.Family)
        # Toolkit.register (Sample.family Sample.combineAll :: Sample.Family)
-}


toolkit :: Toolkit
toolkit
    = Toolkit.register (Sample.family Sample.combineAll)
    $ Toolkit.register (Sum.family Sum.sumBoth)
    $ Toolkit.register Concat.family
    $ Toolkit.register Stateful.family
    $ Toolkit.empty "my-toolkit"