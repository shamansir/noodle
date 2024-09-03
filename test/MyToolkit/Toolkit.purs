module Test.MyToolkit.Toolkit where

import Prelude

import Effect (Effect)

import Noodle.Toolkit (Toolkit) as Noodle
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, FNil, type (//))


import Test.MyToolkit.Node.Sample as Sample
import Test.MyToolkit.Node.Sum as Sum
import Test.MyToolkit.Repr (ISRepr)


type MyFamilies :: Families
type MyFamilies
    =  Sample.F
    // Sum.F
    // FNil


type Toolkit = Noodle.Toolkit MyFamilies ISRepr Effect


{-
toolkit :: Toolkit
toolkit =
    Toolkit.empty "my-toolkit"
        # Toolkit.register (Sum.family Sum.sumBoth :: Sum.Family)
        # Toolkit.register (Sample.family Sample.combineAll :: Sample.Family)
-}


toolkit :: Toolkit
toolkit
    = Toolkit.register (Sample.family Sample.combineAll :: Sample.Family)
    $ Toolkit.register (Sum.family Sum.sumBoth :: Sum.Family)
    $ Toolkit.empty "my-toolkit"