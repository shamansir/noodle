module Blessed where

import Prelude (Unit, unit, pure)
import Foreign (Foreign)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Map (Map)
import Data.Maybe (Maybe)
import Type.Row (type (+))


import Blessed.Command


-- import Blessed.UI.Node (Node(..))
import Blessed.UI.Node as Node
import Blessed.UI.Screen as Screen


data Event = Event


type Blessed =
    { nodes :: Map Node.Id (SNode Foreign Event)
    , lastId :: Int
    }


data BlessedOp m a
    = Lift (m a)
    | PerformOne Node.Id Command a
    | PerformSome Node.Id (Array Command) a


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (r :: Row Type) w e = Array (Prop r e) -> Array (SNode w e) -> SNode w e
type Leaf (r :: Row Type) w e = Array (Prop r e) -> SNode w e

data Handler e = Handler

data SProp w = SProp w
data SNode w e = Snode (Array (SProp w)) (Array (Handler e))


data Prop (r :: Row Type) a = Prop a


run :: forall m. MonadEffect m => SNode Foreign Event -> m Unit
run _ = pure unit


runAnd :: forall m a. MonadEffect m => SNode Foreign Event -> BlessedOp m a -> m Unit
runAnd _ _ = pure unit


screen :: forall w e. Array (Prop Screen.OptionsU e) -> Array (Leaf () w e) -> SNode w e
screen opts children =
    Snode [] []


-- screenAnd :: ScreenOptions () -> Array Node -> (Node -> BlessedOp m a) -> m Unit
-- screenAnd  _ _ _ = pure unit
