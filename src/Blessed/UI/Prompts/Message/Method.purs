module Blessed.UI.Prompts.Message.Method where


import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA

import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Core (method) as C


{- TODO -}

{-
display :: forall m. String -> Number -> Callback -> C.NodeId -> BlessedOp m
display text time callback nodeId =
    C.method nodeId "display"
        [ C.arg CA.string text
        , C.arg CA.string value
        ]
-}


{-
error :: forall m. String -> Number -> Callback -> C.NodeId -> BlessedOp m
error text time callback nodeId =
    C.method nodeId "error"
        [ C.arg CA.string text
        , C.arg CA.string value
        ]
-}
