module Toolkit.Hydra where

import Prelude

import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))

import Toolkit.Hydra.Op (Hydra(..))
import Toolkit.Hydra.Queue (Queue)
import Toolkit.Hydra.Queue (empty) as Queue
import Toolkit.Hydra.Fn as Fn
import Toolkit.Hydra.Fn.Gen (toNodeFn) as Fn

import Noodle.Fn (imapState) as Fn
--import Noodle.Node (imapState) as Node
import Noodle.Toolkit (Toolkit, empty, registerFn) as T

-- type Toolkit = T.Toolkit (Ref Queue, Ref Canvas) Hydra
type Toolkit = T.Toolkit Queue Hydra


toolkit :: Toolkit
toolkit =
    T.registerFn
        (T.empty "Hydra" Queue.empty None)
        unit
        -- FIXME: we have `m == Evaluate` in the toolkit, so we don't need `imapState` and may be Patch State at all.
        --        unless we always want to render something w/o having any node (may be just have a hidden one?)
        (Fn.imapState ((/\) Queue.empty) Tuple.snd $ Fn.toNodeFn Fn.noise)