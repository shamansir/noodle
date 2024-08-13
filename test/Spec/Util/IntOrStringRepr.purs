module Test.Spec.Util.IntOrStringRepr where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Repr (class ToRepr, class FromRepr, class HasFallback)
import Data.Repr (wrap, unwrap) as Repr



data ISRepr
    = None
    | Int Int
    | Str String


derive instance Eq ISRepr


instance Show ISRepr where
    show =
        case _ of
            None -> "<None>"
            Int n -> show n
            Str str -> str
instance HasFallback ISRepr where
    fallback = None
instance ToRepr Int ISRepr where toRepr = Just <<< Repr.wrap <<< Int
instance ToRepr String ISRepr where toRepr = Just <<< Repr.wrap <<< Str
instance FromRepr ISRepr Int where
    fromRepr = Repr.unwrap >>>
        case _ of
            Int n -> Just n
            _ -> Nothing
instance FromRepr ISRepr String where
    fromRepr = Repr.unwrap >>>
        case _ of
            Str str -> Just str
            _ -> Nothing
