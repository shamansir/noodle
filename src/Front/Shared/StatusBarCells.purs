module Front.Shared.StatusBarCells where

import Prelude
import Web.Components.AppScreen.UiMode


data Which
    = Zoom
    | WSStatus
    | UiMode

derive instance Eq Which
derive instance Ord Which


allCells :: Array Which
allCells = [ WSStatus, Zoom, UiMode ]


data Output
    = ResetZoom
    | ChangeMode UiModeKey
