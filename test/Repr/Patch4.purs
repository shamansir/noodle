module Test.Repr.Patch4 where

import Prelude

import Noodle.Node2.MapsFolds.Repr (class HasRepr)


data MyRepr
    = Unit_
    | String_ String
    | Int_ Int
    | Bool_ Boolean
    | Other_


instance Show MyRepr
    where
        show Unit_ = "Unit"
        show (String_ str) = "String::" <> str
        show (Int_ int) = "Int::" <> show int
        show (Bool_ bool) = "Bool_::" <> show bool
        show Other_ = "Other"


instance Eq MyRepr
    where
        eq Unit_ Unit_ = true
        eq (String_ strA) (String_ strB) = strA == strB
        eq (Int_ intA) (Int_ intB) = intA == intB
        eq (Bool_ boolA) (Bool_ boolB) = boolA == boolB
        eq Other_ Other_ = true
        eq _ _ = false


instance HasRepr String MyRepr where toRepr _ = String_
instance HasRepr Int MyRepr where toRepr _ = Int_
instance HasRepr Unit MyRepr where toRepr _ _ = Unit_
instance HasRepr Boolean MyRepr where toRepr _ = Bool_