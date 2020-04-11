module FSM.Nest where


import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))

import FSM


foo = 42

{-
nest
    :: forall actionA modelA actionB modelB
     . FSM actionA modelA
    -> FSM (Either actionA actionB) (modelA /\ modelB)
nest (FSM srcF) =
    FSM dstF
    where
        dstF (Left actionA) (modelA /\ modelB) =
            case srcF actionA modelA of
                modelA' /\ effects' ->
                    (modelA' /\ modelB) /\ effects'
        dstF (Right actionB) (modelA /\ modelB) =
            case srcF actionA modelA of
                modelA' /\ effects' ->
                    (modelA' /\ modelB) /\ effects'
-}
