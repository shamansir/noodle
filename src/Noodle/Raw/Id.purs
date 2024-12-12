module Noodle.Raw.Id where


import Noodle.Id (FamilyR, unsafeFamilyR)
import Noodle.Raw.Fn.Shape


inletR :: String -> InletR
inletR = unsafeInletR


outletR :: String -> OutletR
outletR = unsafeOutletR


familyR :: String -> FamilyR
familyR = unsafeFamilyR