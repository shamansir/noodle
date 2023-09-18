module Test.JsExprParsing.Wrap where

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
import Toolkit.Hydra2.Types (AudioBin) as T
import Toolkit.Hydra2.Repr.Wrap
import Toolkit.Hydra2.Repr.Wrap (WrapRepr(..)) as W
import Toolkit.Hydra2.Types (AudioBin(..), Values(..), GlslFn(..)) as T
import Toolkit.Hydra2.Lang.Fn as Lang


import Data.FromToFile (encode, decode)

import Toolkit.Hydra2.Repr.Wrap.Parser (inlineExprParser)

-- TODO: use fuzzy generator
samples :: Array (String /\ JsExpr)
samples =
    [ "2" /\ Val (Number 2.0)
    , "15.0" /\ Val (Number 15.0)
    , "Math.PI" /\ Val Pi
    , "2 + 2" /\ AddE (Val (Number 2.0)) (Val (Number 2.0))
    , "2 * mouse.x" /\ MulE (Val (Number 2.0)) (Val MouseX)
    , "mouse.x * 0.0003" /\ MulE (Val MouseX) (Val (Number 0.0003))
    , "0.1 + mouse.x * 0.002" /\ AddE (Val (Number 0.1)) (MulE (Val MouseX) (Val (Number 0.002)))
    , "Math.PI / mouse.x" /\ DivE (Val Pi) (Val MouseX)
    , "Math.PI / 2 * mouse.x" /\ MulE (DivE (Val Pi) $ Val $ Number 2.0) (Val MouseX)
    , "Math.PI * mouse.x /180" /\ MulE (Val Pi) (DivE (Val MouseX) (Val (Number 180.0)))
    , "width" /\ Val Width
    , "15 / width + 20" /\ AddE (DivE (Val (Number 15.0)) (Val Width)) (Val (Number 20.0))
    , "(15 / width) + 20" /\ AddE (Brackets $ DivE (Val (Number 15.0)) (Val Width)) (Val (Number 20.0))
    , "Math.sin(time)" /\ Math "sin" (Just $ Val Time)
    , "Math.cos(height)" /\ Math "cos" (Just $ Val Height)
    , "(1.05 + 0.1 * Math.sin(0.05*time))" /\ Brackets (AddE (Val (Number 1.05)) (MulE (Val (Number 0.1)) (Math "sin" (Just (MulE (Val (Number 0.05)) (Val Time))))))
    , "Math.sin(time/22)" /\ Math "sin" (Just (DivE (Val Time) (Val (Number 22.0))))
    , "time/10" /\ DivE (Val Time) (Val (Number 10.0))
    , "Math.sin(time/27)*.01222+9.89" /\ AddE (MulE (Math "sin" (Just (DivE (Val Time) (Val (Number 27.0))))) (Val (Number 0.01222))) (Val (Number 9.89))
    , "1.5 * Math.sin(0.08 * time)" /\ MulE (Val (Number 1.5)) (Math "sin" (Just (MulE (Val (Number 0.08)) (Val Time))))
    , "a.fft[0]" /\ (Val $ Fft $ T.AudioBin 0)
    , "a.fft[1]*40" /\ MulE (Val $ Fft $ T.AudioBin 1) (Val (Number 40.0))
    , "1 + a.fft[3]" /\ AddE (Val (Number 1.0)) (Val $ Fft $ T.AudioBin 3)
    , "time%360*0.05" /\ ModE (Val Time) (MulE (Val (Number 360.0)) (Val (Number 0.05)))
    , "Math.random()*Math.round(time)" /\ MulE (Math "random" Nothing) (Math "round" (Just $ Val Time))
    -- TODO: arrays
    -- , "[5, 0, 1, 2].fast()" /\ Val (Number 2.0)
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
