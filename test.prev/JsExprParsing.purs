module Test.JsExprParsing where

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

import Toolkit.Hydra.Types
import Toolkit.Hydra.Repr.Wrap
import Toolkit.Hydra.Repr.Wrap (WrapRepr(..)) as W
import Toolkit.Hydra.Types (AudioBin(..), Values(..), GlslFn(..)) as T
import Toolkit.Hydra.Lang.Fn as Lang


import Data.FromToFile (encode, decode)

import Toolkit.Hydra.Lang.SketchParser.JsExpr (JsExpr(..), inlineExprParser)

-- TODO: use fuzzy generator
samples :: Array (String /\ JsExpr)
samples =
    [ "2" /\ INum 2.0
    , "15.0" /\ INum 15.0
    , "Math.PI" /\ Pi
    , "2 + 2" /\ Add (INum 2.0) (INum 2.0)
    , "2 * mouse.x" /\ Mul (INum 2.0) MouseX
    , "mouse.x * 0.0003" /\ Mul MouseX (INum 0.0003)
    , "0.1 + mouse.x * 0.002" /\ Add (INum 0.1) (Mul MouseX (INum 0.002))
    , "Math.PI / mouse.x" /\ Div Pi MouseX
    , "Math.PI / 2 * mouse.x" /\ Mul (Div Pi $ INum 2.0) MouseX
    , "Math.PI * mouse.x /180" /\ Mul Pi (Div MouseX $ INum 180.0)
    , "width" /\ Width
    , "15 / width + 20" /\ Add (Div (INum 15.0) Width) (INum 20.0)
    , "(15 / width) + 20" /\ Add (Brackets $ Div (INum 15.0) Width) (INum 20.0)
    , "Math.sin(time)" /\ Math "sin" (Just Time)
    , "Math.cos(height)" /\ Math "cos" (Just Height)
    , "(1.05 + 0.1 * Math.sin(0.05*time))" /\ Brackets (Add (INum 1.05) (Mul (INum 0.1) (Math "sin" (Just (Mul (INum 0.05) Time)))))
    , "Math.sin(time/22)" /\ Math "sin" (Just (Div Time (INum 22.0)))
    , "time/10" /\ Div Time (INum 10.0)
    , "Math.sin(time/27)*.01222+9.89" /\ Add (Mul (Math "sin" (Just (Div Time (INum 27.0)))) (INum 0.01222)) (INum 9.89)
    , "1.5 * Math.sin(0.08 * time)" /\ Mul (INum 1.5) (Math "sin" (Just (Mul (INum 0.08) Time)))
    , "a.fft[0]" /\ Fft 0
    , "a.fft[1]*40" /\ Mul (Fft 1) (INum 40.0)
    , "1 + a.fft[3]" /\ Add (INum 1.0) (Fft 3)
    , "time%360*0.05" /\ Mod Time (Mul (INum 360.0) (INum 0.05))
    , "Math.random()*Math.round(time)" /\ Mul (Math "random" Nothing) (Math "round" (Just Time))
    -- TODO: arrays
    -- , "[5, 0, 1, 2].fast()" /\ INum 2.0
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
