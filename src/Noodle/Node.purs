module Noodle.Node where

import Prelude


import Noodle.Id as Id
import Noodle.Fn (Fn)
import Noodle.Fn.Shape (Shape, Inlets, Inlets)
import Noodle.Fn.Shape (Raw) as Shape
import Noodle.Fn.Protocol (Protocol)
import Noodle.Fn.Tracker (Tracker)


data Node (f :: Symbol) state (is :: Row Type) (os :: Row Type) repr m
    = Node
        (Id.Node f)
        Shape.Raw
        (Tracker state is os repr)
        (Protocol state is os repr)
        (Fn state is os repr m)


{-
make
    :: forall f state (is :: Row Type) isrl (inlets :: Inlets) (os :: Row Type) osrl (outlets :: Inlets) repr m
     . IsSymbol f
    => NodeOrderedInputs iorder isrl is repr
    => NodeOrderedOutputs oorder osrl os repr
    => MonadEffect m
    => Family f
    -> state
    -> Shape inlets outlets
    -> Record is
    -> Record os
    -> ProcessM state is os repr m Unit
    -> m (Node f state is os repr m)
make family state iorder oorder is os process =
    make' (family' family) state is os $ Fn.make (reflect family) { inputs : iorder, outputs : oorder } process
-}