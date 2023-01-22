module Blessed.UI.Base.Element.Method where

import Prelude

import Data.Maybe (Maybe)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

import Blessed.Core.Label (Label)
import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.JsApi (NodeId) as C
import Blessed.Internal.Core (method) as C



-- key
-- onceKey
-- unkey
-- onScreenEvent
-- removeScreenEvent
-- on 'mouse'
-- on 'keypress'


render :: forall m. C.NodeId -> BlessedOp m
render nodeId =
    C.method nodeId "render" [ ]



hide :: forall m. C.NodeId -> BlessedOp m
hide nodeId =
    C.method nodeId "hide" [ ]



show :: forall m. C.NodeId -> BlessedOp m
show nodeId =
    C.method nodeId "show" [ ]



toggle :: forall m. C.NodeId -> BlessedOp m
toggle nodeId =
    C.method nodeId "toggle" [ ]



focus :: forall m. C.NodeId -> BlessedOp m
focus nodeId =
    C.method nodeId "focus" [ ]



free :: forall m. C.NodeId -> BlessedOp m
free nodeId =
    C.method nodeId "free" [ ]



destroy :: forall m. C.NodeId -> BlessedOp m
destroy nodeId =
    C.method nodeId "destroy" [ ]



setIndex :: forall m. Int -> C.NodeId -> BlessedOp m
setIndex z nodeId =
    C.method nodeId "setIndex" [ C.arg CA.int z ]



setFront :: forall m. C.NodeId -> BlessedOp m
setFront nodeId =
    C.method nodeId "setFront" [ ]



setBack :: forall m. C.NodeId -> BlessedOp m
setBack nodeId =
    C.method nodeId "setBack" [ ]



setLabel :: forall m. Label -> C.NodeId -> BlessedOp m
setLabel label nodeId =
    C.method nodeId "setLabel" [ encodeJson label ]



removeLabel :: forall m. C.NodeId -> BlessedOp m
removeLabel nodeId =
    C.method nodeId "removeLabel" [ ]



{- TODO
setHover :: forall m. Options -> C.NodeId -> BlessedOp m
setHover options nodeId =
    C.method nodeId "setHover" [ C.arg CA.options options ]
-}


removeHover :: forall m. C.NodeId -> BlessedOp m
removeHover nodeId =
    C.method nodeId "removeHover" [ ]



enableMouse :: forall m. C.NodeId -> BlessedOp m
enableMouse nodeId =
    C.method nodeId "enableMouse" [ ]



enableKeys :: forall m. C.NodeId -> BlessedOp m
enableKeys nodeId =
    C.method nodeId "enableKeys" [ ]



enableInput :: forall m. C.NodeId -> BlessedOp m
enableInput nodeId =
    C.method nodeId "enableInput" [ ]



enableDrag :: forall m. C.NodeId -> BlessedOp m
enableDrag nodeId =
    C.method nodeId "enableDrag" [ ]



disableDrag :: forall m. C.NodeId -> BlessedOp m
disableDrag nodeId =
    C.method nodeId "disableDrag" [ ]



screenshot :: forall m. C.NodeId -> BlessedOp m
screenshot nodeId =
    C.method nodeId "screenshot" [ ]



screenshotArea :: forall m. Int -> Int -> Int -> Int -> C.NodeId -> BlessedOp m
screenshotArea xi xl yi yl nodeId =
    C.method nodeId "screenshot" [ C.arg CA.int xi, C.arg CA.int xl, C.arg CA.int yi, C.arg CA.int yl ]


{-
render
hide
show
toggle
focus
free
destroy
setIndex z:Int
setFront
setBack
setLabel label:Label
removeLabel
setHover options:Options
removeHover
enableMouse
enableKeys
enableInput
enableDrag
disableDrag
screenshot
screenshotArea xi:Int xl:Int yi:Int yl:Int
-}
