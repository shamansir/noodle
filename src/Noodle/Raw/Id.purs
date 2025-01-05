module Noodle.Raw.Id where


import Noodle.Id (NodeR, FamilyR, unsafeFamilyR, unsafeNodeR)
import Data.UniqueHash (UniqueHash)
import Noodle.Raw.Fn.Shape


nodeR :: FamilyR -> UniqueHash -> NodeR
nodeR = unsafeNodeR


inletR :: String -> InletR
inletR = unsafeInletR


outletR :: String -> OutletR
outletR = unsafeOutletR


familyR :: String -> FamilyR
familyR = unsafeFamilyR