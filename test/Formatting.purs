module Test.Formatting where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Tuple.Nested (type (/\), (/\))

import Blessed.Tagger (Tag)
import Blessed.Tagger as T

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


samples :: Array (Tag /\ String)
samples =
    [ T.s "foo" /\ "foo"
    , T.bolds "test" /\ "{bold}test{/bold}"
    ]


spec :: Spec Unit
spec = do

  describe "Formatting works properly for Blessed" $ do

    foldlWithIndex
        (\idx prev (tag /\ expected) -> do
            prev
            *>
            (it ("works for sample " <> show idx <> " : " <> expected) $
                T.render tag `shouldEqual` expected
            )
        )
        (pure unit)
        samples
