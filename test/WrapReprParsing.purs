module Test.WrapReprParsing where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions

import Test.Generating (parses)

import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Repr.Wrap


samples :: Array WrapRepr
samples =
    [ Unit unit
    , Value $ Number 2.0
    ]


spec :: Spec Unit
spec = do

  describe "Parses NDF File properly" $ do

    foldlWithIndex
        (\idx prev repr -> do
            prev
            *>
            (it ("works for sample " <> show idx) $ do
                "0" `shouldEqual` (show idx)
            )
        )
        (pure unit)
        samples

    {- it "parsing works" $
      parses sampleNdfText expectedNdf NdfFile.parser -}
