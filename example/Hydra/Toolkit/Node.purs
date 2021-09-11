module Hydra.Toolkit.Node where


import Prelude

import Effect.Console as Console

import Data.Tuple.Nested ((/\))
import Data.Maybe (maybe)
import Data.Traversable (sequence)

import Hydra (Hydra(..))
import Hydra as Hydra
import Hydra.Engine as HydraE
import Hydra.Toolkit.Shape (texture, buffer, value, modifier) as Channel
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
      (withInlets
        ~< "1" /\ Channel.value
        ~< "2" /\ Channel.value
        ~< "3" /\ Channel.value
        ~< "4" /\ Channel.value
        ~< "5" /\ Channel.value
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


palette :: Def Hydra
palette =
    Def.define
      (withInlets
        ~< "texture" /\ Channel.texture
        ~< "palette" /\ (Channel.modifier # Channel.hidden)
      )
      (withOutlets
        >~ "palette" /\ Channel.texture
      )
      $ \inlets ->
          Def.pass'
            [ "palette" /\
                (Tex
                  <$> ( Hydra.addModifier
                        <$> (HydraE.entity =<< "texture" <+ inlets)
                        <*> (HydraE.modifier =<< "palette" <+ inlets)
                      )
                )
            ]


solidPalette :: Def Hydra -- TODO: + palette-solid
solidPalette =
    Def.define
      (withInlets
        ~< "palette" /\ (Channel.texture # Channel.hidden)
      )
      (withOutlets
        >~ "palette" /\ Channel.texture
      )
      Def.passThrough


out :: Def Hydra
out =
    Def.define
      (withInlets
         ~< "texture" /\ Channel.texture
      )
      noOutlets
      Def.doNothing


toBuffer :: Def Hydra
toBuffer =
    Def.define
      (withInlets
         ~< "texture" /\ Channel.texture
      )
      noOutlets
      Def.doNothing


fromBuffer :: Def Hydra
fromBuffer =
    Def.define
      noInlets
      (withOutlets
        >~ "texture" /\ Channel.texture
      )
      Def.doNothing


render :: Def Hydra
render =
  Def.define
      noInlets
      noOutlets
      Def.doNothing


pi :: Def Hydra
pi =
    Def.define
      noInlets
      (withOutlets
        >~ "pi" /\ Channel.value
      )
      Def.doNothing