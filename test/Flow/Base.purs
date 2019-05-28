module RpdTest.Flow.Base
    ( MyRpd
    , Delivery(..)
    , sumCursesToApplesNode, sumCursesToApplesNode'
    ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.List ((:))
import Data.List as List

import Rpd.API (Rpd) as R
import Rpd.Network (Network) as R
import Rpd.Process (ProcessF) as R


data Delivery
  = Damaged
  | Email
  | Letter
  | Parcel
  | TV
  | IKEAFurniture
  | Car
  | Notebook
  | Curse Int
  | Liver
  | Banana
  | Apple Int
  | Pills

derive instance genericDelivery :: Generic Delivery _

instance showDelivery :: Show Delivery where
  show = genericShow

instance eqDelivery :: Eq Delivery where
  eq = genericEq


type MyRpd = R.Rpd (R.Network Delivery)


-- producingNothingNode :: R.NodeDef Delivery
-- producingNothingNode =
--   { name : "Nothing"
--   , inletDefs : List.Nil
--   , outletDefs : List.Nil
--   , process : Nothing
--   }


appleOutlet :: String -> R.OutletDef Delivery
appleOutlet label =
  { label
  , accept : pure onlyApples
  }


onlyApples :: Delivery -> Boolean
onlyApples (Apple _) = true
onlyApples _ = false


sumCursesToApplesNode :: R.ProcessF Delivery -> R.NodeDef Delivery
sumCursesToApplesNode processF =
  { name : "Sum Curses to Apples"
  , inletDefs
      : curseInlet "curse2"
      : curseInlet "curse1"
      : List.Nil
  , outletDefs
      : appleOutlet "apples"
      : List.Nil
  , process : processF
  }
  where
    curseInlet label =
      { label
      , accept : pure onlyCurses
      , default : Nothing
      }
    onlyCurses (Curse _) = true
    onlyCurses _ = false


sumCursesToApplesNode' :: R.ProcessF Delivery -> R.NodeDef Delivery
sumCursesToApplesNode' processF =
  let singleOutletNode = sumCursesToApplesNode processF
  in singleOutletNode
      { name = "Sum Curses to Apples'"
      , outletDefs
          = appleOutlet "apples2"
          : appleOutlet "apples1"
          : List.Nil
      }


-- logOrExec
--   :: forall a. Either R.RpdError (Effect a) -> Effect a
-- logOrExec effE =
--   either (log <<< show) identity effE
