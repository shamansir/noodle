module Toolkit.Hydra.UI where

import Prelude (($), (<#>), const, otherwise)

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)

-- import Hydra (Hydra)
-- import Hydra.Toolkit.Generate as Gen
-- import Hydra.Toolkit.Generate (Kind(..))

-- import Hydra.Component.Patch as Patch
-- import Hydra.Component.Node.Num as NumNode
-- import Hydra.Component.Node.Osc as OscNode
-- import Hydra.Component.Node.Color as ColorNode
-- import Hydra.Component.Node.Seq as SeqNode
-- import Hydra.Component.Node.Palette as PaletteNode
-- import Hydra.Component.Node.Buffer as BufferNode
-- import Hydra.Component.Node.Out as OutNode
-- import Hydra.Component.Node.Render as RenderNode
-- import Hydra.Component.Node.Pi as PiNode
-- import Hydra.Component.Node.Math as MathNode
-- import Hydra.Component.Node.Fast as FastNode
-- import Hydra.Toolkit.UI.State (State) as Toolkit
-- import Hydra.Toolkit.UI.Action (Action) as Toolkit
-- import Hydra.Toolkit.UI.Components as UI

import Toolkit.Hydra.Op (Hydra(..), Source(..))
import Toolkit.Hydra.UI.State (State) as Toolkit
import Toolkit.Hydra.UI.Action (Action) as Toolkit
import Toolkit.Hydra.UI.Components as UI


import Noodle.Node as Node
import Noodle.Channel as Channel
-- import Noodle.Channel.Shape as Channel

import App.Toolkit.UI (Markings, GetFlags) as UI
import App.Style as Style

import Color (Color)
import Color as C


data Kind
    = Source
    | Geom
    | Color
    | Blend
    | Mod


components ::  UI.Components Hydra
components =
    { patch, node }


markings :: UI.Markings
markings = { node : markNode, channel : markChannel }


getFlags :: UI.GetFlags
getFlags family =
    { controlArea : hasControlArea family
    , hasTitle : hasTitle family
    , hasRibbon : hasRibbon family
    , hasRemoveButton : true
    }


hasTitle :: Node.Family -> Boolean
hasTitle _ = true


hasRibbon :: Node.Family -> Boolean
hasRibbon _ = true


hasControlArea :: Node.Family -> Boolean
hasControlArea "num" = true
hasControlArea "osc" = true
hasControlArea "color" = true
hasControlArea "seq" = true
hasControlArea "palette" = true
hasControlArea "solid-pal" = true
hasControlArea "to-buffer" = true
hasControlArea "from-buffer" = true
hasControlArea "render" = true
hasControlArea "math" = true
hasControlArea "pi" = true
hasControlArea "fast" = true
hasControlArea _ = false


patch :: forall d. Maybe (UI.PatchComponent d)
patch = Nothing -- FIXME: Just Patch.component


node :: forall d. Node.Family -> Maybe (UI.NodeComponent d)
-- node "num"         = Just $ NumNode.component
-- node "pi"          = Just $ PiNode.component
-- node "osc"         = Just $ OscNode.component
-- node "color"       = Just $ ColorNode.component
-- node "seq"         = Just $ SeqNode.component
-- node "palette"     = Just $ PaletteNode.component PaletteNode.Modifier
-- node "solid-pal"   = Just $ PaletteNode.component PaletteNode.Solid
-- node "to-buffer"   = Just $ BufferNode.component BufferNode.ToBuffer
-- node "from-buffer" = Just $ BufferNode.component BufferNode.FromBuffer
-- node "out"         = Just $ OutNode.component
-- node "render"      = Just $ RenderNode.component
-- node "math"        = Just $ MathNode.component
-- node "fast"        = Just $ FastNode.component
-- node "pi"          = Just $ PiNode.component
node _             = Nothing


-- #404E4D -- 64 78 77
-- #63595C -- 99 89 92
-- #646881 -- 100 104 129
-- #62BEC1 -- 98 190 193
-- #5AD2F4 -- 90 210 244
-- #F7CE5B -- 247 206 91
-- #F7B05B -- 247 176 91
-- #E3879E -- 227 135 158
-- #FEC0CE -- 254 192 206
-- #DDE392 -- 221 227 146


-- source : hsl(20, 100%, 70%) -- 255 153 102 -- #FF9966
-- geometry : hsl(80, 100%, 70%) -- 204 255 102 -- #CCFF66
-- color : hsl(140, 100%, 70%) -- 102 255 153 -- #66FF99
-- blend : hsl(200, 100%, 70%) -- 102 204 255 -- #66CCFF
-- modulate : hsl(260, 100%, 70%) -- 153 102 255 -- #9966FF


valueColor = C.rgb 255 255 255 :: Color
textureColor = C.rgb 98 190 193 :: Color
bufferColor = C.rgb 221 227 146 :: Color


sourceColor = C.rgb 255 153 102 :: Color
geomColor = C.rgb 204 255 102 :: Color
colorColor = C.rgb 102 255 153 :: Color
blendColor = C.rgb 102 204 255 :: Color
modColor = C.rgb 153 102 255 :: Color


isValueNode :: Node.Family -> Boolean
isValueNode "num" = true
isValueNode "time" = true
isValueNode "mouse" = true
isValueNode "seq" = true
isValueNode _ = false


markNode :: Node.Family -> Maybe Color
markNode family | isValueNode family = Just valueColor
markNode "solid-pal" = Just sourceColor
markNode "palette" = Just colorColor
markNode _ = Nothing
{-
markNode family | otherwise = Gen.ofKind family <#> markByKind
    where
        markByKind Source = sourceColor
        markByKind Geom = geomColor
        markByKind Color = colorColor
        markByKind Blend = blendColor
        markByKind Mod = modColor
-}

markChannel :: Channel.Id -> Maybe Color
markChannel "value" = Just valueColor
markChannel "texture" = Just textureColor
markChannel "buffer" = Just bufferColor
markChannel _ = Nothing