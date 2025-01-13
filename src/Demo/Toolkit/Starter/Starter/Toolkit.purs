module Starter.Toolkit where

import Prelude
import Effect (Effect)
import Color as Color
import Data.Maybe (Maybe(..))
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Type.Proxy (Proxy(..))
import Control.Monad.State (class MonadState)
import Noodle.Id (toolkitR, family, FamilyR, unsafeGroupR, group) as Id
import Noodle.Fn.ToFn (fn, class PossiblyToFn)
import Noodle.Fn.ToFn (in_, inx_, out_, outx_, toChanneled) as Fn
import Noodle.Toolkit (Toolkit, ToolkitKey, class MarkToolkit, class IsToolkit, class HasChRepr, class FromPatchState, markGroup)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Repr.ChRepr (ValueInChannel)
import Cli.Class.CliRenderer (class CliRenderer, class CliEditor)
import StarterTk.Simple.Bang as Simple.Bang
import StarterTk.Simple.Metro as Simple.Metro
import StarterTk.Simple.Gennum as Simple.Gennum
import StarterTk.Simple.Random as Simple.Random
import StarterTk.Simple.Knob as Simple.Knob
import StarterTk.Simple.Color as Simple.Color
import StarterTk.Simple.Letter as Simple.Letter
import StarterTk.Simple.Sum as Simple.Sum
import StarterTk.Simple.Lerp as Simple.Lerp
import StarterTk.Simple.Log as Simple.Log
import StarterTk.P5.Shape as P5.Shape
import StarterTk.P5.Sketch as P5.Sketch
import StarterTk.Spreads.Nspread as Spreads.Nspread
import StarterTk.Spreads.Vspread as Spreads.Vspread
import StarterTk.Spreads.Cspread as Spreads.Cspread
import StarterTk.Spreads.Xsshape as Spreads.Xsshape
import Demo.Toolkit.Starter.Repr.StRepr (StateRepr)
import Demo.Toolkit.Starter.Repr.ChRepr (ValueRepr)
import Data.Tuple.Nested ((/\), type (/\))
import Demo.Toolkit.Starter.Repr.ChRepr as VR
import Demo.Toolkit.Starter.Starter.Patch (PState)


type StarterFamilies :: Families
type StarterFamilies = Simple.Bang.F :> Simple.Metro.F :> Simple.Gennum.F :> Simple.Random.F
  :> Simple.Knob.F
  :> Simple.Color.F
  :> Simple.Letter.F
  :> Simple.Sum.F
  :> Simple.Lerp.F
  :> Simple.Log.F
  :> P5.Shape.F
  :> P5.Sketch.F
  :> Spreads.Nspread.F
  :> Spreads.Vspread.F
  :> Spreads.Cspread.F
  :> Spreads.Xsshape.F
  :> TNil

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
  # Toolkit.register Simple.Lerp.family
  # Toolkit.register Simple.Sum.family
  # Toolkit.register Simple.Letter.family
  # Toolkit.register Simple.Color.family
  # Toolkit.register Simple.Knob.family
  # Toolkit.register Simple.Random.family
  # Toolkit.register Simple.Gennum.family
  # Toolkit.register Simple.Metro.family
  # Toolkit.register Simple.Bang.family

instance HasChRepr STARTER ValueRepr
instance IsToolkit STARTER where
  name _ = "Starter"
  groupOf _ = Id.family
    >>>
      ( case _ of
          "bang" -> "simple"
          "metro" -> "simple"
          "gennum" -> "simple"
          "random" -> "simple"
          "knob" -> "simple"
          "color" -> "simple"
          "letter" -> "simple"
          "sum" -> "simple"
          "lerp" -> "simple"
          "log" -> "simple"
          "shape" -> "p5"
          "sketch" -> "p5"
          "nspread" -> "spreads"
          "vspread" -> "spreads"
          "cspread" -> "spreads"
          "xsshape" -> "spreads"
          _ -> "unknown"
      )
    >>> Id.unsafeGroupR

instance CliRenderer STARTER StarterFamilies ValueRepr m where
  cliSize _ _ _ _ _ = Nothing
  cliSizeRaw _ _ _ _ _ = Nothing
  renderCli _ _ _ _ _ = pure unit
  renderCliRaw _ _ _ _ _ = pure unit


instance CliEditor STARTER ValueRepr where
  editorFor _ _ _ _ _ = VR.editorFor


instance MarkToolkit STARTER where
  markGroup _ = Id.group >>>
    ( case _ of
        "simple" -> Color.rgb 6 90 181
        "p5" -> Color.rgb 255 163 0
        "spreads" -> Color.rgb 190 18 80
        _ -> Color.rgb 255 255 255
    )
  markFamily ptk = const <<< markGroup ptk


instance FromPatchState STARTER PState StateRepr where
  loadFromPatch :: Proxy _ -> Id.FamilyR -> PState -> Maybe StateRepr
  loadFromPatch _ _ _ = Nothing


instance PossiblyToFn STARTER (ValueInChannel ValueRepr) (ValueInChannel ValueRepr) Id.FamilyR where
  possiblyToFn _ = Id.family >>> case _ of
    "bang" -> Just $ fn "bang" [] [ Fn.outx_ "bang" ]
    "metro" -> Just $ fn "metro"
      [ Fn.in_ "enabled" $ VR.VBool true, Fn.in_ "period" $ VR.VTime (VR.Time { seconds: 0 }) ]
      [ Fn.outx_ "bang" ]
    "gennum" -> Just $ fn "gennum" [] [ Fn.out_ "out" $ VR.VNumber 0.0 ]
    "random" -> Just $ fn "random" [ Fn.inx_ "bang", Fn.inx_ "min", Fn.inx_ "max" ]
      [ Fn.outx_ "random" ]
    "knob" -> Just $ fn "knob" [ Fn.inx_ "min", Fn.inx_ "max" ] [ Fn.out_ "number" $ VR.VNumber 0.0 ]
    "color" -> Just $ fn "color" [ Fn.inx_ "r", Fn.inx_ "g", Fn.inx_ "b" ]
      [ Fn.out_ "color" $ VR.VColor (VR.Color { r: 0, g: 0, b: 0, a: 255 }) ]
    "letter" -> Just $ fn "letter" [ Fn.inx_ "code" ] [ Fn.outx_ "letter" ]
    "sum" -> Just $ fn "sum" [ Fn.inx_ "a", Fn.inx_ "b", Fn.inx_ "c" ]
      [ Fn.out_ "sum" $ VR.VNumber 0.0 ]
    "lerp" -> Just $ fn "lerp" [ Fn.inx_ "v", Fn.inx_ "min", Fn.in_ "max" $ VR.VNumber 100.0 ]
      [ Fn.out_ "v" $ VR.VNumber 0.0 ]
    "log" -> Just $ fn "log" [ Fn.inx_ "what" ] []
    "shape" -> Just $ fn "shape" [] [ Fn.out_ "shape" $ VR.VShape VR.Circle ]
    "sketch" -> Just $ fn "sketch"
      [ Fn.in_ "shape" $ VR.VShape VR.Circle
      , Fn.in_ "wavescount" $ VR.VNumber 5.0
      , Fn.in_ "startcolor" $ VR.VColor (VR.Color { r: 0, g: 0, b: 0, a: 255 })
      , Fn.in_ "endcolor" $ VR.VColor (VR.Color { r: 0, g: 0, b: 0, a: 255 })
      , Fn.in_ "xspasing" $ VR.VNumber 16.0
      , Fn.in_ "amplitude" $ VR.VNumber 75.0
      , Fn.in_ "period" $ VR.VNumber 500.0
      ]
      []
    "nspread" -> Just $ fn "nspread"
      [ Fn.in_ "min" $ VR.VNumber (-150.0)
      , Fn.in_ "max" $ VR.VNumber 150.0
      , Fn.in_ "count" $ VR.VNumber 26.0
      ]
      [ Fn.outx_ "spread" ]
    "vspread" -> Just $ fn "vspread" [ Fn.inx_ "x", Fn.inx_ "y" ] [ Fn.outx_ "spread" ]
    "cspread" -> Just $ fn "cspread"
      [ Fn.inx_ "red", Fn.inx_ "green", Fn.inx_ "blue", Fn.inx_ "alpha" ]
      [ Fn.outx_ "color" ]
    "xsshape" -> Just $ fn "xsshape"
      [ Fn.inx_ "pos", Fn.inx_ "color", Fn.inx_ "size", Fn.inx_ "angle" ]
      [ Fn.outx_ "shape" ]
    _ -> Nothing
    >>> map Fn.toChanneled
