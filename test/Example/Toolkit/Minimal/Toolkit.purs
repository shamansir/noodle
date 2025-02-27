module Example.Toolkit.Minimal.Toolkit where

import Prelude

import Data.Maybe (Maybe(..))

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Type.Proxy (Proxy(..))

import Effect (Effect)

import Noodle.Id (FamilyR, toolkitR, family) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey) as Noodle
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Toolkit.Families (Families, F)

import Noodle.Fn.Signature (sig, class PossiblyToSignature)
import Noodle.Fn.Signature (in_, inx_, out_, outx_, toChanneled) as Sig
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Example.Toolkit.Minimal.Node.Sample as Sample
import Example.Toolkit.Minimal.Node.Sum as Sum
import Example.Toolkit.Minimal.Node.Concat as Concat
import Example.Toolkit.Minimal.Node.Stateful as Stateful
import Example.Toolkit.Minimal.Repr (MinimalStRepr, MinimalVRepr)
import Example.Toolkit.Minimal.Repr (MinimalVRepr(..)) as MR


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
    $ Toolkit.empty minimalTk (Id.toolkitR "my-toolkit")


instance PossiblyToSignature MINIMAL (ValueInChannel MinimalVRepr) (ValueInChannel MinimalVRepr) Id.FamilyR where
    possiblyToSignature _ = Id.family >>> case _ of
        "sample" -> Just
            $ sig "sample"
                [ Sig.in_ "foo" $ MR.Int 1, Sig.in_ "c" $ MR.Int 2, Sig.in_ "bar" $ MR.Str "5" ]
                [ Sig.out_ "foo" $ MR.Str "1", Sig.out_ "bar" $ MR.Int 12 ]
        "sum" -> Just
            $ sig "sum"
                [ Sig.in_ "a" $ MR.Int 0, Sig.in_ "b" $ MR.Int 0 ]
                [ Sig.out_ "sum" $ MR.Int 0 ]
        "concat" -> Just
            $ sig "concat"
                [ Sig.in_ "left" $ MR.Str "", Sig.in_ "right" $ MR.Str "" ]
                [ Sig.out_ "out" $ MR.Str "", Sig.out_ "len" $ MR.Int 0 ]
        "stateful" -> Just
            $ sig "stateful"
                [ Sig.in_ "a" $ MR.Int 0, Sig.in_ "b" $ MR.Int 0 ]
                [ Sig.out_ "sum" $ MR.Int 0 ]
        _ -> Nothing
        >>> map Sig.toChanneled
