module Toolkit.Hydra.Fn.Gen
  ( Evaluate(..)
  , Fn, toNodeFn
  , fn2v
  )
  where


import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..), maybe)
import Data.Map.Extra (type (/->))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array
import Data.String as String
import Data.Foldable (foldl)
import Data.Vec (Vec, (!!), (+>))
import Data.Vec (fromArray, toArray, zipWithE, singleton, empty) as Vec
import Data.Typelevel.Num.Reps (D0, D1, D2, D3, D4, D5, D6, d0, d1, d2, d3, d4, d5, d6)


import Noodle.Fn (Fn, class ToFn) as Noodle
import Noodle.Fn (make, in_, out_, mapM, mapInputsAndOutputs, mapInputsAndOutputsIds) as Fn
import Noodle.Fn.Process (receive, send, sendIn, lift) as Fn
import Noodle.Channel as Channel
import Noodle.Node as Node
import Noodle.Node (NodeFn)

import Toolkit.Hydra.Op




data Evaluate a = Evaluate Hydra a


type Fn = Noodle.Fn String Hydra String Hydra Unit Evaluate Hydra


eval :: Hydra -> Evaluate Unit
eval h = Evaluate h unit


evaluate :: Evaluate ~> Effect -- or Aff?
evaluate =
    case _ of
        Evaluate hydra a ->
            pure a


toNodeFn :: Fn -> NodeFn Unit Hydra
toNodeFn =
    Fn.mapInputsAndOutputs hydraToDef hydraToDef
    <<< Fn.mapInputsAndOutputsIds Fn.in_ Fn.out_
    <<< Fn.mapM (evaluate >>> liftEffect)
    where
        hydraToDef :: Hydra -> Channel.Def Hydra
        hydraToDef v = Channel.make "foo" v


fn2v
    :: forall a b
     . HydrateIn a => HydrateOut a => HydrateOut b
    => String -> Vec D2 (String /\ a) -> Vec D1 String -> (a -> a -> b) -> Fn
fn2v name inputs output fn =
    Fn.make name
        [ input0id /\ toHydra input0default, input1id /\ toHydra input0default ]
        [ outputid /\ toHydra outputdefault ]
        $ do
            input0val <- Fn.receive input0id
            input1val <- Fn.receive input1id
            let maybeOut = fn <$> fromHydra input0val <*> fromHydra input1val
            maybe (pure unit) (toHydra >>> Fn.send outputid) maybeOut
            maybe (pure unit) (toHydra >>> eval >>> Fn.lift) maybeOut
    where
        input0id = Tuple.fst $ inputs !! d0
        input0default = Tuple.snd $ inputs !! d0
        input1id = Tuple.fst $ inputs !! d1
        input1default = Tuple.snd $ inputs !! d1
        outputid = output !! d0
        outputdefault = fn input0default input1default
