module Blessed where

import Prelude (Unit, unit, pure, identity, (<<<), (<$>), map)
import Foreign (Foreign)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Map (Map)
import Data.Maybe (Maybe)
import Type.Row (type (+))


-- import Blessed.UI.Node (Node(..))
import Blessed.UI.Node as Node
import Blessed.UI.Screen as Screen
import Blessed.Internal.Core as C
import Blessed.Internal.BlessedOp as I


type Event = C.CoreEvent


type State m e =
    { nodes :: Map I.NodeId (C.Blessed m e)
    , lastId :: Int
    }



-- type B e = {}


run :: forall m. MonadEffect m => C.Blessed m Event -> m Unit
run = liftEffect <<< C.execute_


runAnd :: forall m. MonadEffect m => C.Blessed m Event -> I.BlessedOp m -> m Unit
runAnd _ _ = pure unit


screen :: forall r m. String -> C.Node ( Screen.OptionsRow + r ) m Screen.Event
screen = Screen.screen


-- screenAnd :: ScreenOptions () -> Array Node -> (Node -> BlessedOp m a) -> m Unit
-- screenAnd  _ _ _ = pure unit
