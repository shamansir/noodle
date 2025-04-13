module Starter.Toolkit.Expected where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console
import Cli.Class.CliRenderer (class CliRenderer, class CliRawRenderer, class CliEditor)
import Color as Color
import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import StarterTk.Repr.ChRepr (ValueRepr)
import StarterTk.Repr.ChRepr as VR
import StarterTk.Repr.StRepr (StateRepr(..))
import StarterTk.Patch (PState)
import Effect (Effect)
import Noodle.Fn.Signature (sig, class PossiblyToSignature)
import Noodle.Fn.Signature (in_, inx_, out_, outx_, toChanneled) as Sig
import Noodle.Id (Family, FamilyR, family, group, toolkitR, unsafeGroupR, NodeR) as Id
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Toolkit (Toolkit, ToolkitKey, class MarkToolkit, class IsToolkit, class HasChRepr, class FromToPatchState, markGroup)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Node (Node)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Cli.Keys (NodeBoxKey)
import Blessed.Internal.BlessedOp (BlessedOp)
import StarterTk.Library.P5.Shape (F, family) as P5.Shape
import StarterTk.Library.P5.Sketch as P5.Sketch
import StarterTk.Library.Simple.Bang as Simple.Bang
import StarterTk.Library.Simple.Color as Simple.Color
import StarterTk.Library.Simple.Time as Simple.Time
import StarterTk.Library.Simple.Gennum as Simple.Gennum
import StarterTk.Library.Simple.Knob as Simple.Knob
import StarterTk.Library.Simple.Lerp as Simple.Lerp
import StarterTk.Library.Simple.Letter as Simple.Letter
import StarterTk.Library.Simple.Log as Simple.Log
import StarterTk.Library.Simple.Metro as Simple.Metro
import StarterTk.Library.Simple.Random as Simple.Random
import StarterTk.Library.Simple.Sum as Simple.Sum
import StarterTk.Library.Spreads.Cspread as Spreads.Cspread
import StarterTk.Library.Spreads.Nspread as Spreads.Nspread
import StarterTk.Library.Spreads.Vspread as Spreads.Vspread
import StarterTk.Library.Spreads.Xsshape as Spreads.Xsshape
import StarterTk.Body.Cli.P5.Shape (body) as P5.Shape
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil, class Put)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


type StarterFamilies :: Families
type StarterFamilies = Simple.Bang.F :> Simple.Metro.F :> Simple.Gennum.F :> Simple.Random.F
  :> Simple.Knob.F
  :> Simple.Color.F
  :> Simple.Time.F
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
  # Toolkit.register Simple.Time.family
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
          "time" -> "simple"
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

instance MonadEffect m => CliRenderer STARTER StarterFamilies ValueRepr m where
  cliSize _ _ _ _ _ = Nothing
  renderCli
    :: forall (f :: Symbol) fstate is os
     . IsSymbol f
    => RegisteredFamily (F f fstate is os ValueRepr m) StarterFamilies
    => Proxy STARTER
    -> Proxy StarterFamilies
    -> Id.Family f -> NodeBoxKey
    -> Node f fstate is os ValueRepr m
    -> Maybe (BlessedOp fstate m)
  renderCli _ _ family nbkey node = do
    case reflectSymbol (Proxy :: _ f) of
      "shape" -> Just $ do
        unsafeCoerce $ P5.Shape.body (unsafeCoerce family) nbkey (unsafeCoerce node)
      _ -> Nothing


instance MonadEffect m => CliRawRenderer STARTER StarterFamilies ValueRepr m where
  cliSizeRaw _ _ _ _ _ = Nothing
  renderCliRaw _ _ _ _ _ = Nothing


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


instance FromToPatchState STARTER PState StateRepr where
  loadFromPatch :: Proxy _ -> Id.FamilyR -> PState -> StateRepr -> Maybe StateRepr
  loadFromPatch _ familyR _ _ = case Id.family familyR of
    "custom" -> Just StateRepr
    _ -> Nothing
  putInPatch :: Proxy _ -> Id.NodeR -> StateRepr -> PState -> PState
  putInPatch _ _ _ = identity


instance PossiblyToSignature STARTER (ValueInChannel ValueRepr) (ValueInChannel ValueRepr) Id.FamilyR where
  possiblyToSignature _ = Id.family >>> case _ of
    "bang" -> Just $ sig "bang" [] [ Sig.outx_ "bang" ]
    "metro" -> Just $ sig "metro"
      [ Sig.in_ "enabled" $ VR.VBool true, Sig.in_ "period" $ VR.VTime (VR.Time { hours : 0, minutes : 0, seconds: 0 }) ]
      [ Sig.outx_ "bang" ]
    "gennum" -> Just $ sig "gennum" [] [ Sig.out_ "out" $ VR.VNumber 0.0 ]
    "random" -> Just $ sig "random" [ Sig.inx_ "bang", Sig.inx_ "min", Sig.inx_ "max" ]
      [ Sig.outx_ "random" ]
    "knob" -> Just $ sig "knob" [ Sig.inx_ "min", Sig.inx_ "max" ] [ Sig.out_ "number" $ VR.VNumber 0.0 ]
    "color" -> Just $ sig "color" [ Sig.inx_ "r", Sig.inx_ "g", Sig.inx_ "b" ]
      [ Sig.out_ "color" $ VR.VColor (VR.Color { r: 0, g: 0, b: 0, a: 255 }) ]
    "time" -> Just $ sig "time" [ Sig.inx_ "h", Sig.inx_ "m", Sig.inx_ "s" ]
      [ Sig.out_ "time" $ VR.VTime (VR.Time { hours : 0, minutes : 0, seconds : 0 }) ]
    "letter" -> Just $ sig "letter" [ Sig.inx_ "code" ] [ Sig.outx_ "letter" ]
    "sum" -> Just $ sig "sum" [ Sig.inx_ "a", Sig.inx_ "b", Sig.inx_ "c" ]
      [ Sig.out_ "sum" $ VR.VNumber 0.0 ]
    "lerp" -> Just $ sig "lerp" [ Sig.inx_ "v", Sig.inx_ "min", Sig.in_ "max" $ VR.VNumber 100.0 ]
      [ Sig.out_ "v" $ VR.VNumber 0.0 ]
    "log" -> Just $ sig "log" [ Sig.inx_ "what" ] []
    "shape" -> Just $ sig "shape" [] [ Sig.out_ "shape" $ VR.VShape VR.Circle ]
    "sketch" -> Just $ sig "sketch"
      [ Sig.in_ "shape" $ VR.VShape VR.Circle
      , Sig.in_ "wavescount" $ VR.VNumber 5.0
      , Sig.in_ "startcolor" $ VR.VColor (VR.Color { r: 0, g: 0, b: 0, a: 255 })
      , Sig.in_ "endcolor" $ VR.VColor (VR.Color { r: 0, g: 0, b: 0, a: 255 })
      , Sig.in_ "xspasing" $ VR.VNumber 16.0
      , Sig.in_ "amplitude" $ VR.VNumber 75.0
      , Sig.in_ "period" $ VR.VNumber 500.0
      ]
      []
    "nspread" -> Just $ sig "nspread"
      [ Sig.in_ "min" $ VR.VNumber (-150.0)
      , Sig.in_ "max" $ VR.VNumber 150.0
      , Sig.in_ "count" $ VR.VNumber 26.0
      ]
      [ Sig.outx_ "spread" ]
    "vspread" -> Just $ sig "vspread" [ Sig.inx_ "x", Sig.inx_ "y" ] [ Sig.outx_ "spread" ]
    "cspread" -> Just $ sig "cspread"
      [ Sig.inx_ "red", Sig.inx_ "green", Sig.inx_ "blue", Sig.inx_ "alpha" ]
      [ Sig.outx_ "color" ]
    "xsshape" -> Just $ sig "xsshape"
      [ Sig.inx_ "pos", Sig.inx_ "color", Sig.inx_ "size", Sig.inx_ "angle" ]
      [ Sig.outx_ "shape" ]
    _ -> Nothing
    >>> map Sig.toChanneled
