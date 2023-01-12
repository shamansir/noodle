module Blessed where

import Prelude (Unit, unit, pure)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Map (Map)
import Data.Maybe (Maybe)
import Type.Row (type (+))


import Blessed.Command


data Coord
    = Px Int
    | Percent Number
    | Sum Coord Coord
    | Sub Coord Coord


data Offset
    = OCoord Coord
    | Center


type Dimension = Coord


data BorderType
    = Line


type Color = String


type Style =
    ( fg :: Color
    , bg :: Color
    , border :: Record Border
    )


type Border =
    ( type :: BorderType
    , fg :: Color
    , bg :: Color
    )

type ScreenOptions r =
    ( title :: String
    | BoxOptions + r
    )

type BoxOptions r =
    ( top :: Offset
    , left :: Offset
    , width :: Dimension
    , height :: Dimension
    , content :: String -- a ?
    , tags :: Boolean
    , draggable :: Boolean
    , hover :: (Record Style -> Record Style)
    , style :: Record Style
    | r
    )


data Options
    = Box (Record (BoxOptions ()))
    | Screen (Record (ScreenOptions ()))
    | Image


newtype Node =
    Node
        { options :: Options
        -- , parent :: Maybe NodeId
        , children :: Array Node
        }


newtype NodeId = NodeId Int


type Blessed =
    { nodes :: Map NodeId Node
    , lastId :: Int
    }


data BlessedOp m a
    = Lift (m a)
    | PerformOne Command a
    | PerformSome (Array Command) a


run :: forall m a. MonadEffect m => Node -> BlessedOp m a -> m Unit
run _ _ = pure unit