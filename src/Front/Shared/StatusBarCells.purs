module Front.Shared.StatusBarCells where

import Prelude


data Which
    = Zoom
    | WSStatus


derive instance Eq Which
derive instance Ord Which


allCells :: Array Which
allCells = [ WSStatus, Zoom ]


data Output
    = ResetZoom