module Toolkit.Toolkit where

import Prelude ((#))
import Effect (Effect)
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Noodle.Id (toolkitR) as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import StarterTk.Simple.Bang as Simple.Bang
import StarterTk.Simple.Metro as Simple.Metro
import StarterTk.Simple.Random as Simple.Random
import StarterTk.Simple.Knob as Simple.Knob
import StarterTk.Simple.Color as Simple.Color
import StarterTk.Simple.Letter as Simple.Letter
import StarterTk.Simple.Sum as Simple.Sum
import StarterTk.Simple.Log as Simple.Log
import StarterTk.P5.Shape as P5.Shape
import StarterTk.P5.Sketch as P5.Sketch
import StarterTk.Simple.Gennum as Simple.Gennum
import Demo.Toolkit.Starter.Repr (StarterRepr)

type ToolkitFamilies :: Families
type ToolkitFamilies = Simple.Bang.F :> Simple.Metro.F :> Simple.Random.F :> Simple.Knob.F
  :> Simple.Color.F
  :> Simple.Letter.F
  :> Simple.Sum.F
  :> Simple.Log.F
  :> P5.Shape.F
  :> P5.Sketch.F
  :> Simple.Gennum.F
  :> TNil

toolkit :: Toolkit ToolkitFamilies StarterRepr Effect
toolkit = Toolkit.empty (Id.toolkitR "Toolkit") # Toolkit.register Simple.Gennum.family
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
