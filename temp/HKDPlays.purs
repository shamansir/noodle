module HKDPlays where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Control.Monad.Error.Class (throwError)

-- Given this type...
newtype HKD :: (Type -> Type) -> Type
newtype HKD f = HKD (f Int)


-- type Unlift :: Type -> Type
-- type Unlift a = a
data Unlift' :: forall k. k -> Type
data Unlift' a
    -- = Unlift' a
type HKD_Unlift = HKD Unlift' -- Int


-- type Const :: Type -> Type -> Type
-- type Const a b = a
data Const' :: forall a b. a -> b -> Type
data Const' a b
    -- = Const' a
type HKD_ConstBoolean = HKD (Const' Boolean) -- Int


-- the runtime value might be an `a` or not.
type HKD_Maybe = HKD Maybe


type HKD_Either = HKD (Either String)


type HKD_List = HKD List


type HKD_NE_List = HKD NonEmptyList


type HKD_Fn = HKD ((->) String)
type HKD_Fn' = HKD (Function String)


newtype Op a b = Op (b -> a)
type HKD_Op = HKD (Op String)


-- newtype Compose :: forall x y. (x -> Type) -> (y -> x) -> y -> Type
newtype Compose f g a = Compose (f (g a))
-- the runtime value is `Nothing`, or `Just int`.
type HKD_ComposeMaybeMaybe = HKD (Compose Maybe Unlift')
-- the runtime value is `Nothing`, or `Just boolean`.
type HKD_ComposeMaybeMaybe' = HKD (Compose Maybe (Const' Boolean))
-- the runtime value is `Nothing`, `Just Nothing`, or `Just int`.
type HKD_ComposeMaybeMaybe'' = HKD (Compose Maybe Maybe)
-- the runtime value is `Nothing`, `Just (Left string)`, or `Just (Right int)`.
type HKD_ComposeMaybeEither = HKD (Compose Maybe (Either String))
-- the runtime value is `Nothing`, `Just Nil`, or `Just intList`.
type HKD_ComposeMaybeList = HKD (Compose Maybe List)
-- the runtime value is `Nil`, `Cons Nothing Nil`, `Cons (Just i) Nil`, or `...`
type HKD_ComposeListMaybe = HKD (Compose List Maybe)


data Record' :: Row Type -> Type
data Record' rt
data Variant' :: Row Type -> Type
data Variant' rt
newtype HKD_Row f = HKD_Row (f (name :: String, age :: Int))
-- the runtime value is a `String` value and an `Int` value
--   { name :: String, age :: Int}`
type HKD_Row_Record = HKD_Row Record'
-- the runtime value is either a `String` value or an `Int` value
type HKD_Row_Variant = HKD_Row Variant'


type AllTypes :: forall r x. (Row r -> x) -> (Type -> r) -> x
type AllTypes recordOrVariant f =
  recordOrVariant ( name :: f String, age :: f Int )

-- { name :: String, age :: Age }
type PersonRecord = AllTypes Record Unlift'

-- { name :: Boolean, age :: Boolean }
type PersonDisplayLabels = AllTypes Record (Const' Boolean)

-- { name :: Maybe String, age :: Maybe Age }
type PersonSearchLabels = AllTypes Record Maybe

-- Variant (name :: String, age :: Age)
type PersonSingleLabel = AllTypes Variant' Unlift'

-- Variant (name :: Boolean, age :: Boolean)
type PersonToggleLabel = AllTypes Variant' (Const' Boolean)



data InvalidName = InvalidName
data NotPositiveAge = NotPositiveAge
newtype Name = Name String
newtype Age = Age Int

-- Same as `Unlift` but it only "selects" the correct type
type ErrorType  e i o = e
type InputType  e i o = i
type OutputType e i o = o

newtype ErrorType'  e i o = ErrorType' e
newtype InputType'  e i o = InputType' i
newtype OutputType' e i o = OutputType' o


{-
-- type AllTypes' :: (Row Type -> Type) -> (Type -> Type) -> Type
type AllTypes' :: forall k1 k2. (Row k1 -> k2) -> (Type -> Type -> Type -> k1) -> k2
type AllTypes' recordOrVariant f =
  recordOrVariant
    ( name :: f InvalidName String Name
    , age :: f NotPositiveAge Int Age
  --  label :: f errorType inputType outputType
    )

type FormOutputvalues = AllTypes' Record OutputType
type FormErrorsIfAny  = AllTypes' Record (Compose Maybe ErrorType)
type FormInputValues  = AllTypes' Record InputType

-- getName :: FormOutputvalues
-- getName rec = rec.name

-- getNameInput :: FormInputValues
-- getNameInput rec = rec.name

-}

{-
onNameError :: forall m. Monad m => FormErrorsIfAny -> m Unit
onNameError rec = case rec.name of
  Nothing -> pure unit -- no error!
  Just error -> throwError error
-}