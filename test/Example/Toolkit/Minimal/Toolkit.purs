module Example.Toolkit.Minimal.Toolkit where

import Prelude

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Type.Proxy (Proxy(..))

import Effect (Effect)

import Noodle.Id (toolkitR)
import Noodle.Toolkit (Toolkit, ToolkitKey) as Noodle
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F)


import Example.Toolkit.Minimal.Node.Sample as Sample
import Example.Toolkit.Minimal.Node.Sum as Sum
import Example.Toolkit.Minimal.Node.Concat as Concat
import Example.Toolkit.Minimal.Node.Stateful as Stateful
import Example.Toolkit.Minimal.Repr (MinimalStRepr, MinimalVRepr)


foreign import data MINIMAL :: Noodle.ToolkitKey


minimalTk = Proxy :: _ MINIMAL


type MyFamilies :: Families
type MyFamilies
    =  Sample.F
    :> Sum.F
    :> Concat.F
    :> Stateful.F
    :> TNil


type Toolkit = Noodle.Toolkit MINIMAL MyFamilies MinimalStRepr MinimalVRepr Effect


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
    = Toolkit.register Sample.family
    $ Toolkit.register Sum.family
    $ Toolkit.register Concat.family
    $ Toolkit.register Stateful.family
    $ Toolkit.empty minimalTk (toolkitR "my-toolkit")