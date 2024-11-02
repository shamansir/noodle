module ExampleToCode where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.String as String

import Effect (Effect)
import Effect.Console as Console

import Toolkit.Hydra2.Lang (Program)
import Toolkit.Hydra2.Lang.ToCode (toCode, pureScript, javaScript)


import Osc01 as Osc01


tutorialExamples ∷ Array (String /\ Program Unit)
tutorialExamples =
    [ "01-osc" /\ Osc01.example
    ]


functionsExamples :: Array (String /\ Program Unit)
functionsExamples = []


websiteExamples :: Array (String /\ Program Unit)
websiteExamples = []


examples :: Array (String /\ Program Unit)
examples =
    tutorialExamples <> functionsExamples <> websiteExamples



findExample :: String -> Maybe (Program Unit)
findExample _ = Just Osc01.example



main :: Effect Unit
main = do
    case findExample "01-osc" of
        Just example -> do
            Console.log "\nPureScript\n"
            Console.log $ toCode pureScript example
            Console.log "\nJavaScript\n"
            Console.log $ toCode javaScript example
        Nothing ->
            Console.log $ String.joinWith " :: " $ show <$> Tuple.fst <$> examples