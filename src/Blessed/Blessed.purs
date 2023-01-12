module Blessed where

import Prelude (Unit, unit, pure)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Map (Map)
import Data.Maybe (Maybe)
import Type.Row (type (+))


import Blessed.Command


import Blessed.UI.Node (Node(..))
import Blessed.UI.Node as Node
import Blessed.UI.Screen as Screen



type Blessed =
    { nodes :: Map Node.Id Node
    , lastId :: Int
    }


data BlessedOp m a
    = Lift (m a)
    | PerformOne Node.Id Command a
    | PerformSome Node.Id (Array Command) a


run :: forall m a. MonadEffect m => Node -> BlessedOp m a -> m Unit
run _ _ = pure unit


screen :: Screen.Options -> Array Node -> Node
screen opts children =
    Node { options : Node.Screen opts, children }


-- screenAnd :: ScreenOptions () -> Array Node -> (Node -> BlessedOp m a) -> m Unit
-- screenAnd  _ _ _ = pure unit
