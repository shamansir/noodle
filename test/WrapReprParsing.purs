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


-- TODO: use fuzzy generator
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
    , Texture $ Start $ Voronoi { scale : Width, speed : Pi, blending : MouseX }
    , Texture $ Start $ Osc { frequency : Number 60.0, offset : Pi, sync : Width }
    , Texture $ Start $ Load Output3
    , Texture $ Start $ External Source0 $ Camera 2
    , Texture $ Start $ External Source0 Video
    , Texture $ Start $ External Source0 $ Sketch "foobar"
    , Texture $ Filter Empty $ Posterize { bins : Time, gamma : Height }
    , Texture $ Filter (Start $ Load Output2) $ Shift { r : Number 1.5, g : Pi, b : Width, a : Time }
    , Texture $ Filter (Start $ Noise { scale : Number 1.5, offset : MouseX }) $ B { scale : Width, offset : Number 0.0 }
    , Texture
        $ ModulateWith
            { what : Empty
            , with : Empty
            }
        $ ModKaleid { nSides : Number 7.0 }
    , Texture
        $ ModulateWith
            { what : Empty
            , with : Start $ Osc { frequency : Number 60.0, offset : Number 0.0, sync : Number 1.0 }
            }
        $ ModKaleid { nSides : Number 7.0 }
    , Texture
        $ ModulateWith
            { what : Start $ Osc { frequency : Number 60.0, offset : Pi, sync : Number 1.0 }
            , with : Filter (Start $ Noise { scale : Number 1.5, offset : MouseX }) $ B { scale : Width, offset : Number 0.0 }
            }
        $ ModScroll { scrollX : Number 2.0, speedX : Number 1.0, scrollY : Number 0.5, speedY : Pi }
    , Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModRepeat { repeatX : Number 2.5, repeatY : Number 1.0, offsetX : Number 5.0, offsetY : Number 2.0 }
    , Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModRepeatX { offset : Number 2.5, reps : Number 1.0 }
    , Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModScrollY { scrollY : Number 2.5, speed : Number 1.0 }
    , Texture
        $ ModulateWith { what : Empty, with : Empty }
        $ ModScroll { scrollY : Number 2.5, scrollX : Number 1.0, speedY : Number 5.0, speedX : Number 2.0 }
    , Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Diff
    , Texture
        $ BlendOf { what : Start $ Load Output3, with : Filter Empty $ Posterize { bins : Time, gamma : Height } }
        $ Diff
    , Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Layer $ Number 2.0
    , Texture
        $ BlendOf { what : Empty, with : Empty }
        $ Mult $ Number 0.5
    ]

spec :: Spec Unit
spec = do

  describe "Works for all samples" $ do

    foldlWithIndex
        (\idx prev sample -> do
            prev
            *>
            (it ("works for sample " <> show idx <> " : " <> show sample) $
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
