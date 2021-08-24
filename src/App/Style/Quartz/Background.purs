module App.Style.Quartz.Background where

import Data.Color (rgb, rgba) as C

import App.Style (BackgroundStyle)

bg :: BackgroundStyle
bg = { fill : C.rgb 34 34 42 }