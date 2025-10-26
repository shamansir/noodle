module Web.Components.SidePanel.NextActions where

import Prelude

import Data.Array (concat) as Array
import Data.Tuple (uncurry) as Tuple
import Data.Set (toUnfoldable) as Set

import Type.Proxy (Proxy(..))

import Web.Components.SidePanel (SidePanel)

import Data.Text.Format as T

import Front.Shared.HelpText (Context(..), helpText)


panelId = Proxy :: _ "next-actions"


sidePanel :: SidePanel "next-actions" Context Unit
sidePanel =
    { title : "next-actions"
    , char : const '/'
    , value : const unit
    , next : pure <<< render
    }


render :: Context -> Array T.Tag
render (Context context) =
    T.s <$>
    ( Array.concat
         $ Tuple.uncurry helpText
        <$>
         Set.toUnfoldable context
    )