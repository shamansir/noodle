module Foo.Toolkit where

import Prelude ((#))
import Effect (Effect)
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Noodle.Id (toolkitR) as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import MyToolkit.Simple.Bang as Simple.Bang
import MyToolkit.Simple.Metro as Simple.Metro
import MyToolkit.Simple.Random as Simple.Random
import MyToolkit.Simple.Knob as Simple.Knob
import MyToolkit.Simple.Color as Simple.Color
import MyToolkit.Simple.Letter as Simple.Letter
import MyToolkit.Simple.Sum as Simple.Sum
import MyToolkit.Simple.Log as Simple.Log
import MyToolkit.P5.Shape as P5.Shape
import MyToolkit.P5.Sketch as P5.Sketch
import MyToolkit.Simple.Gennum as Simple.Gennum
import Demo.Toolkit.Processing.Repr (ProcessingRepr)

type FooFamilies :: Families
type FooFamilies = Simple.Bang.F :> Simple.Metro.F :> Simple.Random.F :> Simple.Knob.F
  :> Simple.Color.F
  :> Simple.Letter.F
  :> Simple.Sum.F
  :> Simple.Log.F
  :> P5.Shape.F
  :> P5.Sketch.F
  :> Simple.Gennum.F
  :> TNil

toolkit :: Toolkit FooFamilies ProcessingRepr Effect
toolkit = Toolkit.empty (Id.toolkitR "Foo") # Toolkit.register Simple.Gennum.family
  # Toolkit.register P5.Sketch.family
  # Toolkit.register P5.Shape.family
  # Toolkit.register Simple.Log.family
  # Toolkit.register Simple.Sum.family
  # Toolkit.register Simple.Letter.family
  # Toolkit.register Simple.Color.family
  # Toolkit.register Simple.Knob.family
  # Toolkit.register Simple.Random.family
  # Toolkit.register Simple.Metro.family
  # Toolkit.register Simple.Bang.family
