module Test.Files.CodeGenTest.Hydra.Audio.Show where

import Prelude

import Effect (Effect)
import Hydra.Repr.Wrap (WrapRepr)
import Noodle.Fn.Process as Fn
import Noodle.Fn.Process as Noodle
import Noodle.Fn.Shape (I, O)
import Noodle.Fn.Shape as Noodle
import Noodle.Fn.Shape.Temperament (Cold, Hot)
import Noodle.Id as NId
import Noodle.Node as Noodle
import Noodle.Toolkit.Families as Noodle
import Noodle.Toolkit.Family as Family
import Noodle.Toolkit.Family as Noodle
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Hydra.Types as HT
import Hydra.Repr.Wrap as HW
import Noodle.Fn.ToFn (Fn(..))
import Data.Tuple.Nested ((/\))

{- Generated by Noodle Codegen from Hydra NDF file. Group :: audio. Family :: show.

[[  audio : show :: <audio:Audio {SIL} -> todo:TODO {TODO}> => <> ]]

Toolkit : Hydra. File: ./src/Hydra/hydra.v0.3.ndf -}

_show :: NId.Family "show"
_show = NId.Family

type Inlets = (I "audio" Hot HT.AudioSource :> I "todo" Hot HT.TODO :> TNil) :: Noodle.Inlets
type Outlets = TNil :: Noodle.Outlets
type InletsRow = (audio :: HT.AudioSource, todo :: HT.TODO)
type OutletsRow = ()
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "show" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "show" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "show" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { audio: HT.Silence, todo: HT.TODO }

defaultO :: Record OutletsRow
defaultO = {}

_in_audio = Noodle.Inlet :: _ "audio"
_in_todo = Noodle.Inlet :: _ "todo"

family :: Family
family = Family.make _show (HW.Value HT.None) (Noodle.Shape :: Shape) defaultI defaultO showP

makeNode :: Effect Node
makeNode = Family.spawn family

showP :: Process
showP = pure unit