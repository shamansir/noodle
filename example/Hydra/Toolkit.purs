module Hydra.Toolkit where


import Prelude

import Noodle.Toolkit as T

import Data.Tuple.Nested ((/\))

import Noodle.Node ((<+))
import Noodle.Node.Define (Def)
import Noodle.Node.Define (pass', doNothing, empty, define) as Def
import Noodle.Node.Shape ((>~), (~<), withInlets, withOutlets)

import Noodle.Channel.Shape as Channel

import Hydra (Hydra)
import Hydra as Hydra


toolkit :: T.Toolkit Hydra
toolkit =
  T.make Hydra.default
    [ {- "sum" /\ sumNode -} ]


{- sumNode :: Def Hydra
sumNode =
    Def.define
      (withInlets
         ~< "a" /\ Channel.number 0.0
         ~< "b" /\ Channel.number 0.0
      )
      (withOutlets
         >~ "c" /\ Channel.number 0.0
      )
      $ \inlets ->
          Def.pass'
            [ "c" /\ ((+) <$> "a" <+ inlets
                          <*> "b" <+ inlets
                     )
            ] -}