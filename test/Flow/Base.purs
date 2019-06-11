module RpdTest.Flow.Base
    ( MyRpd
    , Delivery(..), Pipe(..)
    , myToolkit
    , sumCursesToApplesNode, sumCursesToApplesNode'
    ) where

import Prelude

import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Rpd.API (Rpd) as R
import Rpd.Network (Network) as R
import Rpd.Process (ProcessF) as R
import Rpd.Toolkit as R


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


type MyRpd = R.Rpd (R.Network Delivery)


data Pipe
  = Pass
  -- | OnlyApples
  -- | OnlyCurses


instance myChannels :: R.Channels Delivery Pipe where
  default _ = Damaged

  accept _ _ = true
  -- accept OnlyApples (Apple _) = true
  -- accept OnlyCurses (Curse _) = true
  -- accept Pass _ = true
  -- accept _ _ = false

  adapt _ = identity


myToolkit ::  R.Toolkit Delivery Pipe
myToolkit =
  R.Toolkit
    (R.ToolkitName "delivery")
    (const Nothing)
        -- (R.nodes
        --   [ "sumCursesToApples" /\ sumCursesToApplesNode
        --   , "sumCursesToApples'" /\ sumCursesToApplesNode'
        --   ])


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
        [ "curse1" /\ Pass
        , "curse2" /\ Pass
        ] # R.inlets
    , outlets :
        [ "apples" /\ Pass
        ] # R.outlets
    , process : processF
    }


sumCursesToApplesNode' :: R.ProcessF Delivery -> R.NodeDef Delivery Pipe
sumCursesToApplesNode' processF =
  R.NodeDef
    { inlets :
        [ "curse1" /\ Pass
        , "curse2" /\ Pass
        ] # R.inlets
    , outlets :
        [ "apples1" /\ Pass
        , "apples2" /\ Pass
        ] # R.outlets
    , process : processF
    }


-- logOrExec
--   :: forall a. Either R.RpdError (Effect a) -> Effect a
-- logOrExec effE =
--   either (log <<< show) identity effE
