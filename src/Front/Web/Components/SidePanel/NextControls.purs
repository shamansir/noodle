module Web.Components.SidePanel.NextControls where

import Prelude

import Type.Proxy (Proxy(..))

import Web.Components.SidePanel (SidePanel)

import Data.Text.Format as T

import Front.Shared.HelpText (Context, helpText)


panelId = Proxy :: _ "next-actions"


sidePanel :: SidePanel "next-actions" Context Unit
sidePanel =
    { title : "next-actions"
    , char : const '/'
    , value : const unit
    , next : pure <<< map T.s <<< helpText
    }
