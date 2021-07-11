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


slotRadius = 6.0


slotInnerRadius = slotRadius * 0.65


slotInnerShift = (slotRadius - slotInnerRadius - 1.0) / 4.0