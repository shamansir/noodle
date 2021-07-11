module App.Units where


import Prelude


data Size a = Size Int Int a


data Pixels = Pixels


data Cells = Cells


cellWidth = 40.0


cellHeight = 40.0


nodeBodyWidth = cellWidth * 4.0


slotOuterWidth = 50.0


slotOuterHeight = 25.0


slotRadius = 5.0


slotStrokeWidth = 1.5


bodyStrokeWidth = 1.0


bodyCornerRadius = 0.0


bodyShadowShift = 5.0