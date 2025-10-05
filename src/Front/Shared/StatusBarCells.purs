module Front.Shared.StatusBarCells where

import Prelude
import Web.Components.AppScreen.UiMode


data Which
    = Zoom
    | WSStatus
    | UiMode
    | KeyboardCombo

derive instance Eq Which
derive instance Ord Which


allCells :: Array Which
allCells = [ UiMode, KeyboardCombo, WSStatus, Zoom ]


data Output
    = ResetZoom
    | ChangeMode UiModeKey
