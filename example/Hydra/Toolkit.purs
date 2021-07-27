module Hydra.Toolkit where


import Prelude

import Noodle.Toolkit as T

import Data.Tuple.Nested ((/\))

import Noodle.Node ((<+))
import Noodle.Node.Define (Def)
import Noodle.Node.Define (pass', doNothing, empty, define, passThrough) as Def
import Noodle.Node.Shape ((>~), (~<), withInlets, withOutlets, noInlets, noOutlets)

import Noodle.Channel.Shape as Channel
import Noodle.Channel.Shape (Shape')

import Hydra (Hydra(..), Value(..))
import Hydra as Hydra


toolkit :: T.Toolkit Hydra
toolkit =
  T.make Hydra.default
    [ "num" /\ numberNode
    , "osc" /\ oscNode
    , "out" /\ outNode
    ]


numberNode :: Def Hydra
numberNode =
    Def.define
      (withInlets
        ~< "num" /\ (valueChannel # Channel.hidden)
      )
      (withOutlets
        >~ "num" /\ valueChannel
      )
      Def.passThrough


oscNode :: Def Hydra
oscNode =
    Def.define
      (withInlets
        ~< "freq" /\ valueChannel
        ~< "sync" /\ valueChannel
        ~< "offset" /\ valueChannel
      )
      (withOutlets
        >~ "osc" /\ oscChannel
      )
      $ \inlets ->
        Def.pass'
          [ "osc" /\
            (Hydra.tryOsc
                 <$> "freq" <+ inlets
                 <*> "sync" <+ inlets
                 <*> "offset" <+ inlets
            )
          ]


outNode :: Def Hydra
outNode =
    Def.define
      (withInlets
         ~< "src" /\ oscChannel
      )
      noOutlets
      Def.passThrough


valueChannel :: Shape' Hydra
valueChannel =
  Channel.shape''' (Value' $ Num 0.0) isValue
  where isValue (Value' _) = true
        isValue _ = false


oscChannel :: Shape' Hydra
oscChannel =
  Channel.shape''' Hydra.defaultOsc isOsc
  where isOsc (Osc _ _ _) = true
        isOsc _ = false
