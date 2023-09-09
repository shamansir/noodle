module Test.IExprParsing where

import Prelude

import Effect.Console as Console
import Effect.Class (liftEffect)

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions

import Parsing (runParser)

import Test.Generating (parses)

import Toolkit.Hydra2.Types
import Toolkit.Hydra2.Repr.Wrap
import Toolkit.Hydra2.Repr.Wrap (WrapRepr(..)) as W
import Toolkit.Hydra2.Types (AudioBin(..), Values(..), GlslFn(..)) as T
import Toolkit.Hydra2.Lang.Fn as Lang


import Data.FromToFile (encode, decode)

import Toolkit.Hydra2.Lang.SketchParser.IExpr (IExpr(..), inlineExprParser)

-- TODO: use fuzzy generator
samples :: Array (String /\ IExpr)
samples =
    [ "2" /\ INum 2.0
    , "15.0" /\ INum 15.0
    -- , "[5, 0, 1, 2].fast()" /\ INum 2.0
    , "Math.PI" /\ Pi
    , "2 + 2" /\ Add (INum 2.0) (INum 2.0)
    , "2 * mouse.x" /\ Mul (INum 2.0) MouseX
    , "Math.PI / mouse.x" /\ Div Pi MouseX
    , "Math.PI / 2 * mouse.x" /\ Mul (Div Pi $ INum 2.0) MouseX
    , "width" /\ Width
    , "15 / width + 20" /\ Add (Div (INum 15.0) Width) (INum 20.0)
    , "(15 / width) + 20" /\ Add (Brackets $ Div (INum 15.0) Width) (INum 20.0)
    , "Math.sin(time)" /\ Math "sin" (Just Time)
    , "Math.cos(height)" /\ Math "cos" (Just Height)
    , "a.fft[0]" /\ Fft 0
    ]


spec :: Spec Unit
spec = do

  describe "Works for all samples" $ do

    foldlWithIndex
        (\idx prev (sample /\ expectation) -> do
            prev
            *>
            (it ("works for sample " <> show idx <> " : " <> sample) $
                case runParser sample inlineExprParser of
                    Right decoded -> do
                            decoded `shouldEqual` expectation
                    Left error -> fail $ encode sample <> " doesn't decode to " <> show sample <> ", error: " <>  show error
            )
        )
        (pure unit)
        samples

    -- TODO:
--   describe "All merged" $ do
--         case runParser (encode <$> samples)


    {- it "parsing works" $
      parses sampleNdfText expectedNdf NdfFile.parser -}
