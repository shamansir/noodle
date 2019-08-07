module RpdTest.Flow.Base
    ( Network, Actions
    , Delivery(..), Pipe(..), Node(..)
    , myToolkit
    , sumCursesToApplesNode, sumCursesToApplesNode'
    ) where

import Prelude

import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Rpd.Network (Network) as R
import Rpd.Process (ProcessF(..))
import Rpd.Process (ProcessF(..)) as R
import Rpd.Toolkit as R
import Rpd.Toolkit ((>~), (~<), withInlets, withOutlets)
import Rpd.API.Action.Sequence (ActionList) as R


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


instance showDelivery :: Show Delivery where
  show Damaged = "Damaged"
  show Email = "Email"
  show Letter = "Letter"
  show Parcel = "Parcel"
  show TV = "TV"
  show IKEAFurniture = "IKEA Furniture"
  show Car = "Car"
  show Notebook = "Notebook"
  show (Curse n) = "Curses: " <> show n
  show Liver = "Liver"
  show Banana = "Banana"
  show (Apple n) = "Apples: " <> show n
  show Pills = "Pills"


derive instance eqDelivery :: Eq Delivery


type Network = R.Network Delivery Pipe Node
type Actions = R.ActionList Delivery Pipe Node


data Pipe
  = Pass
  -- | OnlyApples
  -- | OnlyCurses


data Node
  = Empty
  | SumCursesToApples
  | SumCursesToApples'


instance myChannels :: R.Channels Delivery Pipe where
  default _ = Damaged

  accept _ _ = true
  -- accept OnlyApples (Apple _) = true
  -- accept OnlyCurses (Curse _) = true
  -- accept Pass _ = true
  -- accept _ _ = false

  adapt _ = identity


myToolkit ::  R.Toolkit Delivery Pipe Node
myToolkit =
  R.Toolkit
    (R.ToolkitName "delivery")
    nodes
  where
    nodes Empty = R.emptyNode
    nodes SumCursesToApples = sumCursesToApplesNode Withhold
    nodes SumCursesToApples' = sumCursesToApplesNode' Withhold


-- producingNothingNode :: R.NodeDef Delivery
-- producingNothingNode =
--   { name : "Nothing"
--   , inletDefs : List.Nil
--   , outletDefs : List.Nil
--   , process : Nothing
--   }


sumCursesToApplesNode :: R.ProcessF Delivery -> R.NodeDef Delivery Pipe
sumCursesToApplesNode processF =
  R.NodeDef
    { inlets :
        withInlets
        ~< "curse1" /\ Pass
        ~< "curse2" /\ Pass
    , outlets :
        withOutlets
        >~ "apples" /\ Pass
    , process : processF
    }


sumCursesToApplesNode' :: R.ProcessF Delivery -> R.NodeDef Delivery Pipe
sumCursesToApplesNode' processF =
  R.NodeDef
    { inlets :
        withInlets
        ~< "curse1" /\ Pass
        ~< "curse2" /\ Pass
    , outlets :
        withOutlets
        >~ "apples1" /\ Pass
        >~ "apples2" /\ Pass
    , process : processF
    }


-- logOrExec
--   :: forall a. Either R.RpdError (Effect a) -> Effect a
-- logOrExec effE =
--   either (log <<< show) identity effE
