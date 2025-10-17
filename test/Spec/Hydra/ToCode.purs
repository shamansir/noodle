module Test.Spec.Hydra.ToCode where

import Prelude

import Effect.Console as Console
import Effect.Class (liftEffect)

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, describe, describeOnly, it)
import Test.Spec.Assertions

import Test.Spec.Util.Parsing (parses)
import Test.Spec.Util.Assertions (shouldEqual) as U
import Test.Spec.Hydra.Samples as HSamples


import Noodle.Fn.Signature (Signature(..))
import Noodle.Fn.Signature (i, o) as Sig
import Noodle.Text.ToCode (toCode)
import Noodle.Text.FromCode (fromCode, SourceError, srcErrorToString)

import HydraTk.Types
import HydraTk.Repr.Show (hShow)
import HydraTk.Repr.Wrap
import HydraTk.Repr.Wrap (WrapRepr(..)) as W
import HydraTk.Types (Ease(..), AudioBin(..), Values(..), GlslFn(..)) as T
-- import Hydra.Lang.Fn as Lang
import HydraTk.Repr.Target (_encode, _decode) as Hydra
import Noodle.Text.ToCode (class ToCode, toCode, toJavaScript)
import Test.Spec.Util.Assertions (shouldEqual, shouldEqualStack) as U

-- import Data.FromToFile (encode, decode)

-- import Toolkit.Hydra.Repr.Wrap.Parser


spec :: Spec Unit
spec = do

  describe "Compiles to JS as expected" $ do

    foldlWithIndex
        (\idx prev programRec -> do
            prev
            *>
            (it ("works for program " <> show idx) $ -- <> " : " <> hShow programRec.program) $
                toJavaScript programRec.program `U.shouldEqualStack` programRec.jsExpr
            )
        )
        (pure unit)
        HSamples.programSamples

    {- TODO
    foldlWithIndex
        (\idx prev sampleRec -> do
            prev
            *>
            (it ("works for sample " <> show idx <> " : " <> hShow sampleRec.sample <> " : " <> Hydra._encode sampleRec.sample) $
                toJavaScript sampleRec.sample `shouldEqual` sampleRec.jsExpr -- TODO
            )
        )
        (pure unit)
        HSamples.wrapSamples

    -}
