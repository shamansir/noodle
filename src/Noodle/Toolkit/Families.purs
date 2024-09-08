module Noodle.Toolkit.Families where


infixr 6 type FCons as //


data Families
foreign import data FCons :: FamilyDef -> Families -> Families
foreign import data FNil :: Families


data FamilyDef
foreign import data F :: Symbol -> Type -> Row Type -> Row Type -> Type -> (Type -> Type) -> FamilyDef


class FamilyExistsIn (family :: FamilyDef) (families :: Families)
instance FamilyExistsIn family (FCons family tail)
else instance (FamilyExistsIn family tail) => FamilyExistsIn family (FCons skipfamily tail)



class PutFamily (family :: FamilyDef) (families :: Families) (families' :: Families) | families -> families'


instance PutFamily family FNil (FCons family FNil)
else instance PutFamily family (FCons some tail) (FCons family (FCons some tail))