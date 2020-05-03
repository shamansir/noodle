module Noodle.Process
    ( ProcessF(..), ProcessST'(..)
    , Receive, Send
    -- , InletInNode, OutletInNode
    -- , InletLabel, OutletLabel
    , InletAlias, OutletAlias
    , TracedItem(..)
    , InletHandler(..), OutletHandler(..)
    , NodeHandlers(..), InletHandlers(..), OutletHandlers(..)
    -- , InletsByIndexFlow(..), OutletsByIndexFlow(..)
    -- , InletsByLabelFlow(..), OutletsByLabelFlow(..)
    -- , InletsByPathFlow(..), OutletsByPathFlow(..)
    , InletsData(..), OutletsData(..)
    -- , InletsMapData(..), OutletsMapData(..)
    , makeProcessST
    )
    where

import Prelude

import Data.Maybe
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Exists (Exists, mkExists, runExists)
import Effect (Effect(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Noodle.Util (Flow, type (/->))
import Noodle.Path

import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef


-- type InletInNode = Int
-- type OutletInNode = Int


-- type InletLabel = Alias
-- type OutletLabel = Alias


type InletAlias = Alias
type OutletAlias = Alias


data TracedItem d
    = FromInlet Alias d
    | FromOutlet Alias d


-- data InletsByIndexFlow d = InletsByIndexFlow (Flow (InletInNode /\ d))
-- data OutletsByIndexFlow d = OutletsByIndexFlow (Flow (OutletInNode /\ d))
-- data InletsByLabelFlow d = InletsByLabelFlow (Flow (Maybe InletLabel /\ d))
-- data OutletsByLabelFlow d = OutletsByLabelFlow (Flow (Maybe (OutletLabel /\ d)))
-- type InletsByPathFlow d = Flow (Maybe InletPath /\ d)
-- type OutletsByPathFlow d = Flow (Maybe (OutletPath /\ d))


data InletsData d = InletsData (Array d)
data OutletsData d = OutletsData (Array d)


-- data InletsMapData key d = InletsMapData (key /-> d)
-- data OutletsMapData key d = OutletsMapData (key /-> d)


data InletHandler d = InletHandler (d -> Effect Unit)
data OutletHandler d = OutletHandler (d -> Effect Unit)
data NodeHandlers d = NodeHandlers (Array (TracedItem d -> Effect Unit)) -- TODO: -> Noodle Unit
data InletHandlers d = Inletandlers (Array (d -> Effect Unit)) -- TODO: -> Noodle Unit
data OutletHandlers d = OutletHandlers (Array (d -> Effect Unit)) -- TODO: -> Noodle Unit


-- TODO: is it possible to achieve the `ProcessF function like this one?:
-- process = do
--    (Number' r) <- receive "r"
--    (Number' g) <- receive "g"
--    (Number' b) <- receive "b"
--    send "color" $ Color r g b


-- TODO: use IAlias -> data / OAlias -> data functions instead, see TODO.md for more info

{-
newtype StateF st = StateF (Tuple st (st -> Effect st))

data X
    = Var1 (Int -> Effect Unit)
    | Var2 (String -> Effect Unit)
    | Var3 (Exists StateF)

handler :: X -> Effect Unit
handler (Var1 f) = f 1
handler (Var2 f) = f ""
handler (Var3 f') = do
    _ <- runExists
                (\(StateF (Tuple initialState f)) -> do
                    _ <- f initialState
                    pure unit
                )
                f'
    pure unit
-}

{-
newtype StateF st = StateF (Tuple st (st -> Effect st))

data X
    = Var1 (Int -> Effect Unit)
    | Var2 (String -> Effect Unit)
    | Var3 (forall st. (Tuple st (st -> Effect st)))

handler :: X -> Effect Unit
handler (Var1 f) = f 1
handler (Var2 f) = f ""
handler (Var3 (Tuple init f)) =
    pure $ ST.run do
        _ <- liftEffect $ f init
        pure unit
-}


type Receive d = InletAlias -> Maybe d
type Send d = OutletAlias -> Maybe d


newtype ProcessST' d st = ProcessST' (st /\ ((st /\ Receive d) -> Effect (st /\ Send d)))


data ProcessF d
    = Withhold
    -- | PassThrough -- TODO
    -- | Process (Receive d -> Effect (Send d))
    -- TODO: one more option to produce Aff (and then cancel it on next iteration)
    -- TODO: one more option to return (OutletAlias -> Maybe (Flow d))
    -- TODO: one more option to be able to cancel effect on the next process iteration (i.e. playing a sound or stop some event flow?)
    -- TODO: one more option to fold with some state (i.e. receive the previous state in the process call)
    -- TODO: one more option w/o any effects
    -- TODO: and so, test them all
    | Process (Receive d -> Effect (Send d))
    | ProcessAff (Receive d -> Aff (Send d))
    | ProcessST (Exists (ProcessST' d))


{-
makeProcessST
    :: forall d st
     . (st /\ ((st /\ Receive d) -> Effect (st /\ Send d)))
    -> Exists (ProcessST' d)
makeProcessST = mkExists <<< ProcessST'
-}

makeProcessST
    :: forall d st
     . st
    -> ((st /\ Receive d) -> Effect (st /\ Send d))
    -> Exists (ProcessST' d)
makeProcessST st process = mkExists $ ProcessST' $ st /\ process


    {-
runProcessST :: forall d. Exists (ProcessST' d) -> Receive d -> Effect Unit
runProcessST f' receive =
    do
        _ <- runExists
                    (\(ProcessST' (Tuple initialState f)) -> do
                        _ <- f (Tuple initialState receive)
                        pure unit
                    )
                    f'
        pure unit
        -}
