module JustSum.Toolkit where


import Prelude (($), (#), (<$>), (<*>), (<#>), (+), Unit, unit, identity)

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe


-- import Noodle.Node.Unit (Node)
-- import Noodle.Node.Unit (make) as Node
import Noodle.Node ((<+))
import Noodle.Node.Define (Def)
import Noodle.Node.Define (pass', doNothing, empty, define) as Def
import Noodle.Node.Shape ((>~), (~<), withInlets, withOutlets)
import Noodle.Channel.Shape as Channel
import Noodle.Toolkit as T
import Data.Functor.Invariant (imap)

import Effect (Effect)


type Data = Number


library :: Def Unit
library =
    Def.empty


default :: Data
default = 0.0


sumNode :: Def Data
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


toolkit :: T.Toolkit Data
toolkit =
  T.make default
    [ "sum" /\ sumNode ]


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
