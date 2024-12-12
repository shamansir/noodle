module Starter.Toolkit where

import Prelude (($), (#), (>>>), (<<<), pure, unit, const)
import Effect (Effect)
import Color as Color
import Data.Maybe (Maybe(..))
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Type.Proxy (Proxy(..))
import Noodle.Id (toolkitR, unsafeGroupR, FamilyR, family, group) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey, class MarkToolkit, class IsToolkit, class HasRepr, markGroup)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Fn.ToFn (class PossiblyToFn, fn)
import Noodle.Fn.ToFn (in_, inx_, out_, outx_) as Fn
import Cli.Class.CliRenderer (class CliRenderer)
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico8
import Noodle.Ui.Cli.Palette.Item as C
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
import Demo.Toolkit.Starter.Repr (StarterRepr(..)) as R

type StarterFamilies :: Families
type StarterFamilies =
  Simple.Bang.F
  :> Simple.Metro.F
  :> Simple.Gennum.F
  :> Simple.Random.F
  :> Simple.Knob.F
  :> Simple.Color.F
  :> Simple.Letter.F
  :> Simple.Sum.F
  :> Simple.Log.F
  :> P5.Shape.F
  :> P5.Sketch.F
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
  # Toolkit.register P5.Sketch.family
  # Toolkit.register P5.Shape.family
  # Toolkit.register Simple.Log.family
  # Toolkit.register Simple.Sum.family
  # Toolkit.register Simple.Letter.family
  # Toolkit.register Simple.Color.family
  # Toolkit.register Simple.Knob.family
  # Toolkit.register Simple.Random.family
  # Toolkit.register Simple.Gennum.family
  # Toolkit.register Simple.Metro.family
  # Toolkit.register Simple.Bang.family

instance IsToolkit STARTER where
  name _ = "Starter"
  groupOf _ = Id.family
    >>> case _ of
          "log" ->    "simple"
          "sum" ->    "simple"
          "letter" -> "simple"
          "color" ->  "simple"
          "knob" ->   "simple"
          "random" -> "simple"
          "metro" ->  "simple"
          "bang" ->   "simple"
          "gennum" -> "simple"
          "xsshape" ->  "spreads"
          "cspread" -> "spreads"
          "vspread" -> "spreads"
          "nspread" -> "spreads"
          "sketch" -> "p5"
          "shape" ->  "p5"
          _ -> "unknown"
    >>> Id.unsafeGroupR

instance MarkToolkit STARTER where
  markGroup  _ = Id.group >>> case _ of
    "simple"  -> C.crepr Pico8.trueBlue
    "spreads" -> C.crepr Pico8.orange
    "p5"      -> C.crepr Pico8.darkRed
    _         -> C.crepr Pico8.darkerBlue
  markFamily ptk = const <<< markGroup ptk

instance CliRenderer STARTER StarterFamilies StarterRepr m where
  cliSize _ _ _ _ _ = Nothing
  cliSizeRaw _ _ _ _ _ = Nothing
  renderCli _ _ _ _ _ = pure unit
  renderCliRaw _ _ _ _ _ = pure unit

instance HasRepr STARTER StarterRepr

instance PossiblyToFn STARTER (Maybe StarterRepr) (Maybe StarterRepr) Id.FamilyR where
  possiblyToFn _ = Id.family >>> case _ of
    "sketch" -> Just $ fn "sketch" [] [ Fn.out_ "number" $ R.VNumber 0.0 ]
    "sum" -> Just $ fn "sum" [ Fn.inx_ "a", Fn.inx_ "b", Fn.inx_ "c" ] [ Fn.out_ "sum" $ R.VNumber 0.0 ]
    -- TODO: code-generate
    _ -> Nothing