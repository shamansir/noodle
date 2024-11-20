module Starter.Toolkit where

import Prelude ((#))
import Effect (Effect)
import Color as Color
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Type.Proxy (Proxy(..))
import Noodle.Id (toolkitR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey, class MarkToolkit)
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
import StarterTk.Spreads.Nspread as Spreads.Nspread
import StarterTk.Spreads.Vspread as Spreads.Vspread
import StarterTk.Spreads.Cspread as Spreads.Cspread
import StarterTk.Spreads.Xsshape as Spreads.Xsshape
import Demo.Toolkit.Starter.Repr (StarterRepr)

type StarterFamilies :: Families
type StarterFamilies = Simple.Bang.F :> Simple.Metro.F :> Simple.Random.F :> Simple.Knob.F
  :> Simple.Color.F
  :> Simple.Letter.F
  :> Simple.Sum.F
  :> Simple.Log.F
  :> P5.Shape.F
  :> P5.Sketch.F
  :> Simple.Gennum.F
  :> Spreads.Nspread.F
  :> Spreads.Vspread.F
  :> Spreads.Cspread.F
  :> Spreads.Xsshape.F
  :> TNil

foreign import data STARTER :: ToolkitKey

toolkit :: Toolkit STARTER StarterFamilies StarterRepr Effect
toolkit = Toolkit.empty (Proxy :: _ STARTER) (Id.toolkitR "Starter")
  # Toolkit.register Spreads.Xsshape.family
  # Toolkit.register Spreads.Cspread.family
  # Toolkit.register Spreads.Vspread.family
  # Toolkit.register Spreads.Nspread.family
  # Toolkit.register Simple.Gennum.family
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

instance MarkToolkit STARTER where
  markGroup _ group = Color.rgb 255 255 255
  markFamily _ family = Color.rgb 255 255 255
