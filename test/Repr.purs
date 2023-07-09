module Test.Repr where

import Prelude

import Data.String.Read (read)
import Data.Maybe (Maybe(..))
import Data.Int as Int

import Data.Repr (Repr(..), class ToRepr)
import Data.Repr as Repr

import Noodle.Node2.MapsFolds.Repr (class HasRepr)
import Noodle.Node2.MapsFolds.Repr as HasRepr



data MyRepr
    = Unit_
    | String_ String
    | Int_ Int
    | Bool_ Boolean
    | Other_


instance Repr.ToRepr String MyRepr where
    toRepr = Repr.exists <<< String_


instance Repr.ToRepr Int MyRepr where
    toRepr = Repr.exists <<< Int_


instance Repr.ToRepr Boolean MyRepr where
    toRepr = Repr.exists <<< Bool_


instance Repr.FromRepr MyRepr Boolean where
    fromRepr (Repr (Bool_ bool)) = Just bool
    fromRepr _                   = Nothing


instance Repr.FromRepr MyRepr Int where
    fromRepr (Repr (Int_ int)) = Just int
    fromRepr _                 = Nothing


instance Repr.FromRepr MyRepr String where
    fromRepr (Repr (String_ str)) = Just str
    fromRepr _                    = Nothing


instance Repr.ReadRepr MyRepr where
    readRepr :: String -> Maybe (_ MyRepr)
    readRepr =
        case _ of
            "unit" -> Just (Repr.Repr Unit_)
            str ->
                case (read str :: Maybe Boolean) of -- <|>
                    Just bool -> Just $ Repr.Repr $ Bool_ bool
                    Nothing ->
                        case (Int.fromString str :: Maybe Int) of -- <|>
                            Just int -> Just $ Repr.Repr $ Int_ int
                            Nothing -> Just $ Repr.Repr $ String_ str



{- instance HasRepr String MyRepr where toRepr _ = String_
instance HasRepr Int MyRepr where toRepr _ = Int_
instance HasRepr Unit MyRepr where toRepr _ _ = Unit_
instance HasRepr Boolean MyRepr where toRepr _ = Bool_ -}


-- instance HasRepr a MyRepr => Repr.ToRepr a MyRepr where toRepr = Repr.exists <<< HasRepr.toRepr


-- instance Repr.ToRepr a MyRepr => HasRepr a MyRepr where toRepr _ = Repr.toRepr



testTo ∷ { a ∷ Maybe (Repr MyRepr) , b ∷ Maybe (Repr MyRepr) , c ∷ Maybe (Repr MyRepr) }
testTo = Repr.toReprRow { a : "foo", b : 15, c : false }


-- testFrom ∷ { a ∷ Maybe String , b ∷ Maybe Int , c ∷ Maybe Boolean }
-- testFrom = Repr.fromReprRow { a : Repr.exists (String_ "foo"), b : Repr.exists (Int_ 15), c : Repr.exists (Bool_ false) }


-- testFrom' = Repr.fromReprRow testTo

-- testFrom = Repr.fromReprRow { a :String_ "foo", b : Int_ 15, c : Bool_ false }