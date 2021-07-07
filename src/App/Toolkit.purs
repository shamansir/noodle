module App.Toolkit where


import Prelude (($), (#), (<$>), (<*>), (<#>), (+), Unit, unit)

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe


-- import Noodle.Node.Unit (Node)
-- import Noodle.Node.Unit (make) as Node
import Noodle.Node ((<+))
import Noodle.Node.Define (pass', doNothing, withFn2) as Def
import Noodle.Node.Shaped (Node)
import Noodle.Node.Shaped as NS
import Noodle.Node.Shape ((>~), (~<), withInlets, withOutlets)
import Noodle.Channel.Shape as Channel
import Noodle.Toolkit.Shaped as TS
import Data.Functor.Invariant (imap)

import Effect (Effect)


library :: NS.Def Unit
library =
    NS.empty


sumNode :: NS.Def Int
sumNode =
    NS.define
      (withInlets
         ~< "a" /\ Channel.int 0
         ~< "b" /\ Channel.int 0
      )
      (withOutlets
         >~ "c" /\ Channel.int 0
      )
      $ \inlets ->
          Def.pass'
            [ "c" /\ (inlets # Def.withFn2 ((+)) "a" "b")
            ]

-- TODO


{- type Data = Maybe Int

data Data
  = None
  | Bang
  | Number Int -}


{-
sumNode' :: NS.Def (Maybe Int)
sumNode' = sumNode # imap ?wh ?wh -- Just (Maybe.fromMaybe 0)
-}


toolkit :: TS.Toolkit Int
toolkit =
  TS.make [ "sum" /\ sumNode ]


{-
toolkit :: TS.Toolkit (Maybe Int)
toolkit =
  TS.make
    [ -- "sum" /\ (sumNode # imap Just (Maybe.fromMaybe 0))
    --, "library" /\ (library # imap ?wh2 ?wh3)
    ]
  -}

-- timerNode -- TODO


-- sineNode -- TODO
