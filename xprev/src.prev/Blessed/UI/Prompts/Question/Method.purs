module Blessed.UI.Prompts.Question.Method where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Core (method) as C


{- TODO -}

{-
ask :: forall m. String -> Callback -> C.NodeId -> BlessedOp m
ask question callback nodeId =
    C.method nodeId "ask"
        [ C.arg CA.string text
        , C.arg CA.string value
        ]
-}
