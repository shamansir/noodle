module Hydra.Toolkit.Node where


import Prelude

import Effect.Console as Console

import Data.Tuple.Nested ((/\))
import Data.Maybe (maybe)
import Data.Traversable (sequence)

import Hydra (Hydra)
import Hydra as Hydra
import Hydra.Engine as HydraE
import Hydra.Toolkit.Shape (entity, out, value) as Channel
import Hydra.Compile (compile) as Hydra
import Hydra.Try as Hydra
import Hydra.Extract as HydraE

import Noodle.Node ((<+))
import Noodle.Node.Define (Def)
import Noodle.Node.Define as Def
import Noodle.Node.Shape ((>~), (~<), withInlets, withOutlets, noInlets, noOutlets)
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


time :: Def Hydra
time =
    Def.define
      noInlets
      (withOutlets
        >~ "time" /\ Channel.value
      )
      $ Def.alwaysOne
      $ "time" /\ Hydra.time


mouse :: Def Hydra
mouse =
    Def.define
      noInlets
      (withOutlets
        >~ "x" /\ Channel.value
        >~ "y" /\ Channel.value
      )
      $ Def.always
          [ "x" /\ Hydra.mouseX
          , "y" /\ Hydra.mouseY
          ]


seq :: Def Hydra
seq =
    Def.define
      (withOutlets
        >~ "1" /\ Channel.value
        >~ "2" /\ Channel.value
        >~ "3" /\ Channel.value
        >~ "4" /\ Channel.value
        >~ "5" /\ Channel.value
      )
      (withOutlets
        >~ "seq" /\ Channel.value
      )
      $ \inlets ->
        Def.pass
          [ "seq" /\
              HydraE.buildSeq5
                  ("1" <+ inlets)
                  ("2" <+ inlets)
                  ("3" <+ inlets)
                  ("4" <+ inlets)
                  ("5" <+ inlets)
          ]



out :: Def Hydra
out =
    Def.defineEffectful
      (withInlets
         ~< "src" /\ Channel.entity
      )
      noOutlets
      $ \inlets -> do
          _ <- sequence $ do
              hydra <- "src" <+ inlets
              compiledStr <- Hydra.compile $ Hydra.out' hydra
              --pure $ pure unit
              pure $ do
                Console.logShow hydra
                HydraE.evaluate compiledStr
          pure $ Def.passNothing