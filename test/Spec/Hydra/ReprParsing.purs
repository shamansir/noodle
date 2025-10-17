module Test.Spec.Hydra.ReprParsing where

import Prelude

import Effect.Console as Console
import Effect.Class (liftEffect)

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, describe, it)
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

-- import Data.FromToFile (encode, decode)

-- import Toolkit.Hydra.Repr.Wrap.Parser


textureSamples :: Array Texture
textureSamples = HSamples.textureSamples


wrapReprSamples :: Array WrapRepr
wrapReprSamples = HSamples.wrapSamples <#> _.sample


spec :: Spec Unit
spec = do

  describe "Works for all Wrap Repr samples" $ do

    foldlWithIndex
        (\idx prev sample -> do
            prev
            *>
            (it ("works for sample " <> show idx <> " : " <> hShow sample <> " : " <> Hydra._encode sample) $
                case (Hydra._decode $ Hydra._encode sample :: Either SourceError WrapRepr) of
                    Right decoded -> do
                        -- liftEffect $ Console.log $ Hydra._encode sample
                        (Hydra._encode decoded) `U.shouldEqual` (Hydra._encode sample)
                    Left srcError -> fail $ "\t" <> Hydra._encode sample <> "\n\n\tfailed to decode the following sample:\n\n\t" <> hShow sample <> "\n\n\t" <> srcErrorToString srcError
            )
        )
        (pure unit)
        wrapReprSamples

  describe "Works for all textures" $ do

    foldlWithIndex
        (\idx prev sample -> do
            prev
            *>
            (it ("works for sample " <> show idx <> " : " <> hShow sample {- <> " : " <> Hydra._encode sample-}) $
                case (Hydra._decode $ Hydra._encode sample :: Either SourceError Texture) of
                    Right decoded ->
                        (Hydra._encode decoded) `U.shouldEqual` (Hydra._encode sample)
                    Left srcError -> fail $ "\t" <> Hydra._encode sample <> "\n\n\tfailed to decode the following sample:\n\n\t" <> hShow sample <> "\n\n\t" <> srcErrorToString srcError
            )
        )
        (pure unit)
        textureSamples