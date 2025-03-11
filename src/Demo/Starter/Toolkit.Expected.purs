module Starter.ToolkitExpected where

import Prelude (($), (#), (>>>), (<<<), pure, unit, const, negate)
import Effect (Effect)
import Color as Color
import Data.Maybe (Maybe(..))
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Type.Proxy (Proxy(..))
import Control.Monad.State (class MonadState)
import Noodle.Id (toolkitR, unsafeGroupR, FamilyR, family, group) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey, class MarkToolkit, class IsToolkit, class HasChRepr, markGroup)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Fn.Signature (class PossiblyToSignature, sig)
import Noodle.Fn.Signature (in_, inx_, out_, outx_) as Sig
import Cli.Class.CliRenderer (class CliRenderer, class CliRawRenderer, class CliEditor)
import Noodle.Ui.Cli.Palette.Set.Pico8 as Pico8
import Noodle.Ui.Cli.Palette.Item as C
import StarterTk.Library.Simple.Bang as Simple.Bang
import StarterTk.Library.Simple.Metro as Simple.Metro
import StarterTk.Library.Simple.Random as Simple.Random
import StarterTk.Library.Simple.Knob as Simple.Knob
import StarterTk.Library.Simple.Color as Simple.Color
import StarterTk.Library.Simple.Letter as Simple.Letter
import StarterTk.Library.Simple.Sum as Simple.Sum
import StarterTk.Library.Simple.Log as Simple.Log
import StarterTk.Library.P5.Shape as P5.Shape
import StarterTk.Library.P5.Sketch as P5.Sketch
import StarterTk.Library.Simple.Gennum as Simple.Gennum
import StarterTk.Library.Spreads.Nspread as Spreads.Nspread
import StarterTk.Library.Spreads.Vspread as Spreads.Vspread
import StarterTk.Library.Spreads.Cspread as Spreads.Cspread
import StarterTk.Library.Spreads.Xsshape as Spreads.Xsshape
import StarterTk.Repr.ChRepr (ValueRepr)
import StarterTk.Repr.ChRepr (ValueRepr(..)) as VR
import StarterTk.Repr.ChRepr (Color(..), Time(..), Shape(..)) as RV
import StarterTk.Repr.StRepr (StateRepr)
import StarterTk.Repr.StRepr (StateRepr(..)) as SR

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
  -- TNil

foreign import data STARTER :: ToolkitKey

toolkit :: Toolkit STARTER StarterFamilies StateRepr ValueRepr Effect
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
    "simple"  -> C.colorOf Pico8.trueBlue
    "spreads" -> C.colorOf Pico8.orange
    "p5"      -> C.colorOf Pico8.darkRed
    _         -> C.colorOf Pico8.darkerBlue
  markFamily ptk = const <<< markGroup ptk

instance CliRenderer STARTER StarterFamilies ValueRepr m where
  cliSize _ _ _ _ _ = Nothing
  renderCli _ _ _ _ _ = Nothing


instance CliRawRenderer STARTER StarterFamilies ValueRepr m where
  cliSizeRaw _ _ _ _ _ = Nothing
  renderCliRaw _ _ _ _ _ = Nothing


instance CliEditor STARTER ValueRepr where
  editorFor _ _ _ _ _ _ = Nothing

instance HasChRepr STARTER ValueRepr

instance PossiblyToSignature STARTER (Maybe ValueRepr) (Maybe ValueRepr) Id.FamilyR where
  possiblyToSignature _ = Id.family >>> case _ of
    "bang" -> Just $ sig "bang" [] [ Sig.outx_ "bang" ]
    "metro" -> Just $ sig "metro"
      [ Sig.in_ "enabled" $ VR.VBool true
      , Sig.in_ "period" $ VR.VTime $ RV.Time { hours : 0, minutes : 0, seconds: 0 }
      ]
      [ Sig.outx_ "bang" ]
    "gennum" -> Just $ sig "gennum" [] [ Sig.out_ "out" $ VR.VNumber 0.0 ]
    "random" -> Just $ sig "random" [ Sig.inx_ "bang", Sig.inx_ "min", Sig.inx_ "max" ]
      [ Sig.outx_ "random" ]
    "knob" -> Just $ sig "knob" [ Sig.inx_ "min", Sig.inx_ "max" ] [ Sig.out_ "number" $ VR.VNumber 0.0 ]
    "color" -> Just $ sig "color" [ Sig.inx_ "r", Sig.inx_ "g", Sig.inx_ "b" ]
      [ Sig.out_ "color" $ VR.VColor $ RV.Color { r: 0, g: 0, b: 0, a: 255 } ]
    "letter" -> Just $ sig "letter" [ Sig.inx_ "code" ] [ Sig.outx_ "letter" ]
    "sum" -> Just $ sig "sum" [ Sig.inx_ "a", Sig.inx_ "b", Sig.inx_ "c" ] [ Sig.out_ "sum" $ VR.VNumber 0.0 ]
    "log" -> Just $ sig "log" [ Sig.inx_ "what" ] []
    "shape" -> Just $ sig "shape" [] [ Sig.out_ "shape" $ VR.VShape RV.Circle ]
    "sketch" -> Just $ sig "sketch"
      [ Sig.in_ "shape" $ VR.VShape RV.Circle
      , Sig.in_ "wavescount" $ VR.VNumber 5.0
      , Sig.in_ "startcolor" $ VR.VColor $ RV.Color { r: 0, g: 0, b: 0, a: 255 }
      , Sig.in_ "endcolor" $ VR.VColor $ RV.Color { r: 0, g: 0, b: 0, a: 255 }
      , Sig.in_ "xspasing" $ VR.VNumber 16.0
      , Sig.in_ "amplitude" $ VR.VNumber 75.0
      , Sig.in_ "period" $ VR.VNumber 500.0
      ]
      []
    "nspread" -> Just $ sig "nspread"
      [ Sig.in_ "min" $ VR.VNumber $ -150.0, Sig.in_ "max" $ VR.VNumber 150.0, Sig.in_ "count" $ VR.VNumber 26.0 ]
      [ Sig.outx_ "spread" ]
    "vspread" -> Just $ sig "vspread" [ Sig.inx_ "x", Sig.inx_ "y" ] [ Sig.outx_ "spread" ]
    "cspread" -> Just $ sig "cspread"
      [ Sig.inx_ "red", Sig.inx_ "green", Sig.inx_ "blue", Sig.inx_ "alpha" ]
      [ Sig.outx_ "color" ]
    "xsshape" -> Just $ sig "xsshape"
      [ Sig.inx_ "pos", Sig.inx_ "color", Sig.inx_ "size", Sig.inx_ "angle" ]
      [ Sig.outx_ "shape" ]
    _ -> Nothing
