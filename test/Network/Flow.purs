module RpdTest.Network.Flow
    ( network, MyData ) where

import Prelude

import Rpd as R
import Rpd.Flow (flow) as R

import Data.Map as Map
import Data.Maybe (fromMaybe)

import FRP.Event as Event
import FRP.Event.Time (interval)

data MyData
  = Bang
  | Str' String String
  | Num' String Int

node :: String -> R.LazyNode MyData
node nodeId =
  R.node "f"
    [ R.inlet "a" -- WithDefault "a" (Str' (nodeId <> "a") "i")
    , R.inletWithDefault "b" $ Str' (nodeId <> "b") "test"
    , R.inlet' "f" $ R.flow $ map (Num' (nodeId <> "f")) $ interval 5000
    , R.inlet "d" -- (ST.every ST.second S.~> Num' (nodeId <> "d"))
    , R.inlet "e" -- WithDefault "e" (Num' (nodeId <> "e") 3.0)
    ]
    [ R.outlet "c"
    , R.outlet' "x" $ R.flow
        $ map (Num' (nodeId <> "x"))
        $ Event.fold (\_ n -> n + 1) (interval 5000) 0
    , R.outlet' "y" $ R.flow
        $ map (Num' (nodeId <> "y"))
        $ Event.fold (\_ n -> n + 1) (interval 2000) 0
    ]
    -- (\_ -> [ "c" /\ Int' 10 ] )


network :: R.Network MyData
network =
  R.network
    [ R.patch "Patch One"
      [ node "1"
      , R.processWith processF $ node "2"
      ] -- >>> connect (patch.getNode 0) "a" (patch.getNode 1) "b"
    ]
  where
    processF inputs | Map.isEmpty inputs = Map.empty
    processF inputs | Map.member "d" inputs =
      Map.singleton "c" $ fromMaybe Bang $ Map.lookup "d" inputs
    processF inputs = Map.empty
