module Blessed where

import Prelude (Unit, unit, pure, identity)
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
import Blessed.Internal.Blessed as I


data Event = Event


type State m e =
    { nodes :: Map Node.Id (Blessed m e)
    , lastId :: Int
    }


data Handler m e = Handler

data SProp = SProp String Foreign
data SNode m e = SNode String (Array SProp) (Array (SNode m e)) (Array (Handler m e))


type Blessed m e = SNode m e


data BlessedOp m a
    = Lift (m a)
    | PerformOne Node.Id Command a
    | PerformSome Node.Id (Array Command) a


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (r :: Row Type) m e = Array (Prop r e) -> Array (Blessed m e) -> Blessed m e
type Leaf (r :: Row Type) m e = Array (Prop r e) -> Blessed m e
-- type B e = {}


data Prop (r :: Row Type) e = Prop String Foreign e


run :: forall m. MonadEffect m => Blessed m Event -> m Unit
run _ = pure unit


runAnd :: forall m a. MonadEffect m => Blessed m Event -> BlessedOp m a -> m Unit
runAnd _ _ = pure unit


screen :: forall m e. String -> Node Screen.OptionsU m e
screen name _ _ = SNode name [] [] [] -- TODO


-- screenAnd :: ScreenOptions () -> Array Node -> (Node -> BlessedOp m a) -> m Unit
-- screenAnd  _ _ _ = pure unit
