module Web.Components.SidePanel.Tree where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))

import Web.Components.SidePanel (SidePanel)

import Noodle.Network (Network)
import Noodle.Tree (formatPathTree, toPathTree)

import Web.Components.AppScreen.State as CState


sidePanel :: forall tk p fs sr cr m. SidePanel "tree" (Network tk p fs sr cr m) Boolean
sidePanel =
    { title : "tree"
    , char : const 'T'
    , isOn : identity
    , next : (\network ->
            toPathTree (const unit) network
            <#> \pathTree ->
                true /\ pure (formatPathTree pathTree)
        )
    , onToggle : identity
    }