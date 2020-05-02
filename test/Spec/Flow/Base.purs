module Noodle.Test.Spec.Flow.Base
    ( Network, Actions
    , Delivery(..), Pipe(..), Node(..)
    , myToolkit, mySequencer
    ) where

import Prelude

import FSM
import Noodle.Network (Network) as R
import Noodle.Toolkit as R
import Noodle.API.Action.Sequence (ActionList, Sequencer, make) as R


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
  | Custom


instance myChannels :: R.Channels Delivery Pipe where
  default _ = Damaged

  accept _ _ = true
  -- accept OnlyApples (Apple _) = true
  -- accept OnlyCurses (Curse _) = true
  -- accept Pass _ = true
  -- accept _ _ = false

  adapt _ = identity


myToolkit :: R.Toolkit Delivery Pipe Node
myToolkit =
  R.Toolkit
    (R.ToolkitName "delivery")
    nodes
  where
    nodes Empty = R.emptyNode
    nodes Custom = R.emptyNode


mySequencer :: R.Sequencer Delivery Pipe Node
mySequencer = R.make myToolkit



-- TODO:
-- channelsAfter
--   :: forall d c n
--    . (Show d)
--   => Milliseconds
--   -> ActionList d c n
--   -> Aff (TracedFlow d)
-- channelsAfter delay actions =
--   CollectData.channelsAfter delay myToolkit (Network.empty "network") actions
