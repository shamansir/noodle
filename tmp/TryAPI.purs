module TryAPI where

import Prelude

import Data.Boolean
import Control.Monad.Cont (ContT(..))
import Control.Monad.Except (ExceptT)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)

data Network = Network Unit
data Patch = Patch Unit
data Node = Node Unit
data Inlet = Inlet Unit
data Outlet = Outlet Unit
data Link = Link Unit

type Rpd a = ExceptT String Effect a

data PatchId = PatchId String
data NodePath = NodePath String
data InletPath = InletPath String
data OutletPath = OutletPath String


addNode :: String -> Network -> Rpd (NodePath /\ Network)
addNode _ _ = pure $ (NodePath "") /\ Network unit


connect :: String -> Network -> Rpd Network
connect _ _ = pure $ Network unit


data RpdDSL a next
    = MakeNode {} next
    | MakeNodeAnd {} (Node -> RpdDSL a next) next
    | MakeInlet {} next
    | MakeOutlet {} next
    | Select Node next
    | Connect Inlet Outlet next
    | Bang


buildNetwork :: Rpd Int
buildNetwork = do
    let nw = Network unit
    (nodeA /\ nw') <- addNode "test" nw
    (nodeB /\ nw'') <- addNode "test" nw'
    nw''' <- connect "" nw''
    pure 0


data Hole = Hole1 Int | Hole2 | Hole3 String | Hole29 Int Int


largeProgram :: String -> ContT String Effect Hole
largeProgram v = ContT $ \k -> do
    x <- k (Hole1 0)
    _ <- k Hole2
    _ <- k (Hole3 x)
    _ <- k (Hole29 2 2)
    pure ""


type Context a = a /\ Network


data Msg a
    = AddNode {} (Node -> Context Node)
    | AddInlet Node (Inlet -> Context Inlet)


class DataOf c where
    accept :: forall a. a -> c -> Boolean
    adapt :: forall a. a -> c -> a
    allow :: c -> c -> Boolean


-- connect'' :: forall c. DataOf c => String -> String -> c -> Network -> Network
-- connect'' outlet inlet channel nw = nw

addInlet'' :: forall c. DataOf c => String -> String -> c -> Network -> Network
addInlet'' node id channel nw = nw


network patches = Network unit
patch id nodes = Patch unit
node path def inlets outlets = Node unit
inlet path def = Inlet unit
outlet path def = Outlet unit
patchProjection patch = Node unit

connect' outletPath inletPath nw = nw
removeNode path nw = nw
removeOutlet path nw = nw
send path v nw = nw
addNode' id node nw = nw


nodeDef = {}
channelDef = {}


v =  network
        [ patch "x"
            [ node "foo" nodeDef
                [ inlet "a" channelDef
                , inlet "b" channelDef
                ]
                [ outlet "c" channelDef
                , outlet "d" channelDef
                ]
            , node "bar" nodeDef
                [ inlet "x" channelDef
                ]
                [ outlet "z" channelDef
                ]
            , patchProjection $ patch "z" []
            ]
        , patch "y" []
        ]
     # connect' "x/foo/d" "y/bar/x"
     # removeNode "x/bar"
     # removeOutlet "x/bar/z"
     # send "foo/bar" 5
     # addNode' "y" (node "ax" nodeDef [] [])


run =
    traverse
