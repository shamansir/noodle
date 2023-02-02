module Blessed.UI.Base.Element.Method where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

import Blessed.Core.Label (Label)
import Blessed.Internal.Command (arg) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedSubj (Element, Subject)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.Core (method) as C



-- key
-- onceKey
-- unkey
-- onScreenEvent
-- removeScreenEvent
-- on 'mouse'
-- on 'keypress'


render
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
render nodeId =
    C.method nodeId "render" [ ]


hide
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
hide nodeId =
    C.method nodeId "hide" [ ]



show
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
show nodeId =
    C.method nodeId "show" [ ]



toggle
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
toggle nodeId =
    C.method nodeId "toggle" [ ]



focus
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
focus nodeId =
    C.method nodeId "focus" [ ]



free
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
free nodeId =
    C.method nodeId "free" [ ]



destroy
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
destroy nodeId =
    C.method nodeId "destroy" [ ]



setIndex
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> NodeKey subj id -> BlessedOp state m
setIndex z nodeId =
    C.method nodeId "setIndex" [ C.arg CA.int z ]



setFront
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
setFront nodeId =
    C.method nodeId "setFront" [ ]



setBack
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
setBack nodeId =
    C.method nodeId "setBack" [ ]



setLabel
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Label -> NodeKey subj id -> BlessedOp state m
setLabel label nodeId =
    C.method nodeId "setLabel" [ encodeJson label ]



removeLabel
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
removeLabel nodeId =
    C.method nodeId "removeLabel" [ ]



{- TODO
setHover
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Options -> NodeKey subj id -> BlessedOp state m
setHover options nodeId =
    C.method nodeId "setHover" [ C.arg CA.options options ]
-}


removeHover
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
removeHover nodeId =
    C.method nodeId "removeHover" [ ]



enableMouse
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
enableMouse nodeId =
    C.method nodeId "enableMouse" [ ]



enableKeys
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
enableKeys nodeId =
    C.method nodeId "enableKeys" [ ]



enableInput
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
enableInput nodeId =
    C.method nodeId "enableInput" [ ]



enableDrag
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
enableDrag nodeId =
    C.method nodeId "enableDrag" [ ]



disableDrag
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
disableDrag nodeId =
    C.method nodeId "disableDrag" [ ]



screenshot
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => NodeKey subj id -> BlessedOp state m
screenshot nodeId =
    C.method nodeId "screenshot" [ ]



screenshotArea
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Element subj id
    => Int -> Int -> Int -> Int -> NodeKey subj id -> BlessedOp state m
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
