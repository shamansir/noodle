module Toolkit.Hydra2.Lang where

import Prelude

import Prelude (Unit, unit, discard, (#), pure, bind, ($), (<>))
import Prelude (class Show, show) as Core

import Effect (Effect)
import Effect.Console as Console

import Data.Functor (class Functor)
import Control.Apply (class Apply)
import Control.Bind (class Bind)


import Toolkit.Hydra2.Types



data Command
    = Unknown
    | Batch (Array Command)
    | WithSource Source Command
    | WithFrom From Command
    | WithAudio Audio String Command
    | WithColor ColorOp Command
    | Render Output



data Program a = -- same as Writer?
    Program (Array Command) a


unknown ∷ Program Unit
unknown = Program [ Unknown ] unit


instance Functor Program where
    map :: forall a b. (a -> b) -> Program a -> Program b
    map fa (Program aitems a) =
        Program aitems $ fa a


instance Apply Program where
    apply :: forall a b. Program (a -> b) -> Program a -> Program b
    apply (Program fitems fa) (Program aitems a) =
        Program (fitems <> aitems) $ fa a


instance Bind Program where
    bind :: forall a b. Program a -> (a -> Program b) -> Program b
    bind (Program aitems aval) f =
        case f aval of
            Program bitems b -> Program (aitems <> bitems) b


instance Core.Show Command where
    show cmd = "i"


instance Core.Show a => Core.Show (Program a) where
    show :: Program a -> String
    show (Program items a) = Core.show items <> "    " <> Core.show a


commandsOf :: forall a. Program a -> Array Command
commandsOf (Program items _) = items


s0 ∷ Program Unit
s0 = unknown


o0 ∷ Output
o0 = Output0


initCam _ = unknown

setBins _ _ = unknown


src _ = unknown


a = unknown

show _ = unknown


osc ∷ forall a. Value → Value → Value → Program a → Program Unit
osc frequency sync offset program =
    Program [ WithSource (Osc { frequency, sync, offset }) $ Batch $ commandsOf program ] unit

modulate _ _ _ = unknown


saturate v program =
    Program [ WithColor (Saturate v) $ Batch $ commandsOf program ] unit

pixelate _ _ _ = unknown

scale _ _ = unknown


fn _ = unknown

out :: Output -> Program Unit -> Program Unit
out output program =
    Program [ WithFrom (Output output) $ Batch $ commandsOf program ] unit


fft _ _ _ = unknown


render :: Output -> Program Unit
render out = Program [ Render out ] unit


n :: Number -> Value
n = Number