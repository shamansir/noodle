module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C

import Rpd as Rpd
import Signal.Channel as SC


-- main function with a custom patch

data MyNodeType = NumNode | StrNode

data MyInletType = NumInlet | StrInlet


main :: Eff (console :: C.CONSOLE, channel :: SC.CHANNEL) Unit
main = void do
    Rpd.run [] Rpd.network
    -- Rpd.run
    --     ( Rpd.createNetwork "a" : Rpd.createNetwork "b" : [] )
    --     (\s -> map show s S.~> C.log)
