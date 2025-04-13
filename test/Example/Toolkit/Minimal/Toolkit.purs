module Example.Toolkit.Minimal.Toolkit where

import Prelude

import Data.Maybe (Maybe(..))

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)

import Noodle.Id (FamilyR, NodeR, toolkitR, family) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey) as Noodle
import Noodle.Toolkit (empty, register, class FromToPatchState) as Toolkit
import Noodle.Toolkit.Families (Families, F)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (from, to) as StRepr

import Noodle.Fn.Signature (sig, class PossiblyToSignature)
import Noodle.Fn.Signature (in_, inx_, out_, outx_, toChanneled) as Sig
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Example.Toolkit.Minimal.Node.Sample as Sample
import Example.Toolkit.Minimal.Node.Sum as Sum
import Example.Toolkit.Minimal.Node.Concat as Concat
import Example.Toolkit.Minimal.Node.Stateful as Stateful
import Example.Toolkit.Minimal.Node.ModifiesPatch as ModifiesPatch
import Example.Toolkit.Minimal.ChRepr (MinimalVRepr)
import Example.Toolkit.Minimal.ChRepr (MinimalVRepr(..)) as Ch
import Example.Toolkit.Minimal.StRepr (MinimalStRepr)
import Example.Toolkit.Minimal.StRepr (MinimalStRepr(..)) as St
import Example.Toolkit.Minimal.PatchState (State(..), default) as Patch


foreign import data MINIMAL :: Noodle.ToolkitKey


minimalTk = Proxy :: _ MINIMAL


type MyFamilies :: Families
type MyFamilies
    =  Sample.F
    :> Sum.F
    :> Concat.F
    :> Stateful.F
    :> ModifiesPatch.F
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
    $ Toolkit.register ModifiesPatch.family
    $ Toolkit.empty minimalTk (Id.toolkitR "my-toolkit")


instance Toolkit.FromToPatchState MINIMAL Patch.State ModifiesPatch.State where
    loadFromPatch :: Proxy MINIMAL -> Id.FamilyR -> Patch.State -> ModifiesPatch.State -> Maybe ModifiesPatch.State
    loadFromPatch _ _ (Patch.State { intVal, strVal }) (ModifiesPatch.State (_ /\ str)) = Just $ ModifiesPatch.State $ { intVal, strVal } /\ str
    putInPatch :: Proxy MINIMAL -> Id.NodeR -> ModifiesPatch.State -> Patch.State -> Patch.State
    putInPatch _ _ (ModifiesPatch.State (pstate /\ _)) _ = Patch.State pstate


instance PossiblyToSignature MINIMAL (ValueInChannel MinimalVRepr) (ValueInChannel MinimalVRepr) Id.FamilyR where
    possiblyToSignature _ = Id.family >>> case _ of
        "sample" -> Just
            $ sig "sample"
                [ Sig.in_ "foo" $ Ch.Int 1, Sig.in_ "c" $ Ch.Int 2, Sig.in_ "bar" $ Ch.Str "5" ]
                [ Sig.out_ "foo" $ Ch.Str "1", Sig.out_ "bar" $ Ch.Int 12 ]
        "sum" -> Just
            $ sig "sum"
                [ Sig.in_ "a" $ Ch.Int 0, Sig.in_ "b" $ Ch.Int 0 ]
                [ Sig.out_ "sum" $ Ch.Int 0 ]
        "concat" -> Just
            $ sig "concat"
                [ Sig.in_ "left" $ Ch.Str "", Sig.in_ "right" $ Ch.Str "" ]
                [ Sig.out_ "out" $ Ch.Str "", Sig.out_ "len" $ Ch.Int 0 ]
        "stateful" -> Just
            $ sig "stateful"
                [ Sig.in_ "a" $ Ch.Int 0, Sig.in_ "b" $ Ch.Int 0 ]
                [ Sig.out_ "sum" $ Ch.Int 0 ]
        _ -> Nothing
        >>> map Sig.toChanneled
