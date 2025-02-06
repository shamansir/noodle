module Test.Spec.Util.Assertions where

import Prelude

import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple (fst, snd) as Tuple
import Data.Bifunctor (bimap)

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Effect.Exception (Error, error)
import Test.Spec.Assertions (fail)
import Data.These (these, These)
import Data.Align (align)


class PrettyPrint a where
    prettyPrint :: a -> String


shouldEqual
  :: forall m t
   . MonadThrow Error m
  => Eq t
  => PrettyPrint t
  => t
  -> t
  -> m Unit
shouldEqual v1 v2 =
  when (v1 /= v2) $
    fail $ compareLines (prettyPrint v1) (prettyPrint v2)


compareLines :: String -> String -> String
compareLines a b =
    let
        linesA = String.split (String.Pattern "\n") a
        linesB = String.split (String.Pattern "\n") b
        cmp = these
                (\lA -> "++ " <> lA)
                (\lB -> "-- " <> lB)
                (\lA lB ->
                    if lA == lB then ".. " <> lA
                    else ">> " <> lA <> "\n" <> "<< " <> lB
                )
    in String.joinWith "\n" $ align cmp linesA linesB


instance PrettyPrint String where
    prettyPrint = identity


instance PrettyPrint Int where
    prettyPrint = show


instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (These a b) where
    prettyPrint = bimap prettyPrint prettyPrint >>>
        these
            (\lA -> "< " <> lA)
            (\lB -> "> " <> lB)
            (\lA lB -> ": " <> lA <> " ; " <> lB)


instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Tuple a b) where
    prettyPrint tpl = prettyPrint (Tuple.fst tpl) <> "\n" <> prettyPrint (Tuple.snd tpl)



instance PrettyPrint a => PrettyPrint (Array a) where
    prettyPrint arr = String.joinWith "\n" $ prettyPrint <$> arr
