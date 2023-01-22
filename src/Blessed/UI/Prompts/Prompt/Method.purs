module Blessed.UI.Prompts.Prompt.Method where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C


input :: forall m. String -> String -> C.NodeId -> BlessedOp m
input text value nodeId =
    C.method nodeId "input"
        [ C.arg CA.string text
        , C.arg CA.string value
        ]


{-
input' :: forall m. String -> String -> Callback -> C.NodeId -> BlessedOp m
input' text value nodeId =
    C.method nodeId "input"
        [ C.arg CA.string text
        , C.arg CA.string value
        ]
-}