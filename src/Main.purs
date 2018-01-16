module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Function (apply, applyFlipped)
import Rpd as Rpd
import Signal as S
import Signal.Time as ST

-- Elm-style operators

infixr 0 apply as <|
infixl 1 applyFlipped as |>

-- test stuff

hello :: S.Signal String
hello = (ST.every 1000.0) S.~> show

helloEffect :: forall eff. S.Signal (Eff (console :: CONSOLE | eff) Unit)
helloEffect = hello S.~> log

main_ :: forall eff. Eff (console :: CONSOLE | eff) Unit
main_ = S.runSignal helloEffect

-- main function with a custom patch

data MyNodeType = NumNode | StrNode

data MyInletType = NumInlet | StrInlet

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main =
    let
        network = Rpd.init "network"
            |> Rpd.addPatch "foo" "bar"
    in
        S.runSignal (Rpd.log network S.~> log)
    -- let
        -- patch = createPatch' "foo"
        -- node = createNode' "num" NumNode
        -- inlet = createInlet' "foo" StrInlet
        -- nodeWithInlet = addInlet' inlet node
        -- (Patch _ sumSignal) = addNode' nodeWithInlet patch
        -- signalLog = S.runSignal ((stringRenderer patch) S.~> log)
    -- in
        -- S.runSignal helloEffect
