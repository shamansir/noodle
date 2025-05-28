module Web.Components.SidePanel.Tree where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\), type (/\))

import Web.Components.SidePanel (SidePanel)

import Noodle.Network (Network)
import Noodle.Tree (formatPathTree, toPathTree)

import Web.Components.AppScreen.State as CState


panelId = Proxy :: _ "tree"


sidePanel :: forall tk p fs sr cr m. SidePanel "tree" (Network tk p fs sr cr m) Unit
sidePanel =
    { title : "tree"
    , char : const 'T'
    , value : const unit
    , next : (\network ->
            toPathTree (const unit) network
            <#> \pathTree -> pure (formatPathTree pathTree)
        )
    }