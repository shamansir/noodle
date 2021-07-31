module Hydra.Toolkit.Node where


import Prelude

import Effect.Console as Console

import Data.Tuple.Nested ((/\))
import Data.Maybe (maybe)
import Data.Traversable (sequence)

import Hydra (Hydra)
import Hydra as Hydra
import Hydra.Engine as HydraE

import Hydra.Toolkit.Shape (osc, value) as Channel
import Hydra.Compile (compile) as Hydra

import Noodle.Node ((<+))
import Noodle.Node.Define (Def)
import Noodle.Node.Define (define, defineEffectful, pass', passThrough, passNothing) as Def
import Noodle.Node.Shape ((>~), (~<), withInlets, withOutlets, noOutlets)
import Noodle.Channel.Shape (hidden) as Channel


number :: Def Hydra
number =
    Def.define
      (withInlets
        ~< "num" /\ (Channel.value # Channel.hidden)
      )
      (withOutlets
        >~ "num" /\ Channel.value
      )
      Def.passThrough


osc :: Def Hydra
osc =
    Def.define
      (withInlets
        ~< "freq" /\ Channel.value
        ~< "sync" /\ Channel.value
        ~< "offset" /\ Channel.value
      )
      (withOutlets
        >~ "osc" /\ Channel.osc
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


out :: Def Hydra
out =
    Def.defineEffectful
      (withInlets
         ~< "src" /\ Channel.osc
      )
      noOutlets
      $ \inlets -> do
          _ <- sequence $ do
              hydra <- ("src" <+ inlets)
              compiledStr <- Hydra.compile $ Hydra.out' hydra
              --pure $ pure unit
              pure $ HydraE.evaluate compiledStr
          pure $ Def.passNothing