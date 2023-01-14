module Blessed where

import Prelude (Unit, unit, pure, identity, (<<<))
import Foreign (Foreign)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Map (Map)
import Data.Maybe (Maybe)
import Type.Row (type (+))


-- import Blessed.UI.Node (Node(..))
import Blessed.UI.Node as Node
import Blessed.UI.Screen as Screen
import Blessed.Internal.Core as I


data Event = Event


type State m e =
    { nodes :: Map I.NodeId (I.Blessed m e)
    , lastId :: Int
    }



-- type B e = {}


run :: forall m. MonadEffect m => I.Blessed m Event -> m Unit
run = liftEffect <<< I.execute_


runAnd :: forall m a. MonadEffect m => I.Blessed m Event -> I.BlessedOp m a -> m Unit
runAnd _ _ = pure unit


screen :: forall r m e. String -> I.Node ( Screen.OptionsRow + r ) m e
screen name props children = I.SNode (I.NodeId name) (I.lockProps props) children [] -- TODO


-- screenAnd :: ScreenOptions () -> Array Node -> (Node -> BlessedOp m a) -> m Unit
-- screenAnd  _ _ _ = pure unit
