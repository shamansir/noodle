module Test.WrapReprParsing where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions

import Test.Generating (parses)

import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Repr.Wrap
import Toolkit.Hydra2.Types (AudioBin(..), Values(..)) as T


import Data.FromToFile (encode, decode)

import Toolkit.Hydra2.Repr.Wrap.Parser


samples :: Array WrapRepr
samples =
    [ Unit unit
    , Value $ Number 2.0
    , Value None
    , Value Undefined
    , Value Time
    , Value MouseX
    , Value MouseY
    , Value Width
    , Value Height
    , Value Pi
    , Value $ Fft $ T.AudioBin 1
    , Value $ VArray (T.Values []) Linear
    , Value $ VArray (T.Values [ MouseX, Time, Pi, Number 4.5, None, Pi ]) Linear
    , Value $ VArray (T.Values []) $ Fast $ Number 7.0
    , Value $ VArray (T.Values []) $ Fit { low : Number 1.0, high : Number 2.5 }
    -- , Value $ Dep $ VExpr $ Val $ Number 3.0
    , Texture Empty
    , Texture $ Start $ Gradient { speed : Number 2.0 }
    , Texture $ Start $ Noise { scale : Number 1.5, offset : MouseX }
    , Texture $ Start $ Solid { r : Number 1.5, g : Pi, b : Width, a : Time }
    , Texture $ Start $ Load Output3
    , Texture $ Start $ External Source0 $ Camera 2
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
