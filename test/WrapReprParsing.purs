module Test.WrapReprParsing where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions

import Test.Generating (parses)

import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Repr.Wrap


import Data.FromToFile (encode, decode)

import Toolkit.Hydra2.Repr.Wrap.Parser


samples :: Array WrapRepr
samples =
    [ Unit unit
    , Value $ Number 2.0
    ]

spec :: Spec Unit
spec = do

  describe "Works for all samples" $ do

    foldlWithIndex
        (\idx prev sample -> do
            prev
            *>
            (it ("works for sample " <> show idx) $
                case (decodeImpl $ encode sample :: Maybe WrapRepr) of
                    Just decoded ->
                        {-case decoded `maybeEq` sample of
                            Just false -> fail $ encode sample <> " doesn't decode to " <> show sample
                            Just true -> pure unit
                            Nothing -> -} (encode decoded) `shouldEqual` (encode sample)
                    Nothing -> fail $ encode sample <> " doesn't decode to " <> show sample
            )
        )
        (pure unit)
        samples

    {- it "parsing works" $
      parses sampleNdfText expectedNdf NdfFile.parser -}
