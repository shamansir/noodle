module Cli.State.NwWraper where

import Prelude (Unit, (>>>))

import Noodle.Network2 (Network) as Noodle

import Toolkit.Hydra2 as Hydra

type NoodleNetwork m = Noodle.Network Hydra.State (Hydra.Families m) (Hydra.Instances m)


newtype Network m =  -- compiler 0.14.5 fails without newtype
    Network (NoodleNetwork m)
-- derive instance Newtype (Network m) _ -- fails


unwrapN :: forall m. Network m -> NoodleNetwork m
unwrapN (Network nw') = nw'


wrapN :: forall m. NoodleNetwork m -> Network m
wrapN = Network


withNetwork :: forall m. (NoodleNetwork m -> NoodleNetwork m) -> Network m -> Network m
withNetwork f = unwrapN >>> f >>> wrapN
