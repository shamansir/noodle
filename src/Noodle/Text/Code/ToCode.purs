module Noodle.Text.ToCode where

import Prelude hiding (show)

-- import Toolkit.Hydra.Types
-- import Toolkit.Hydra.Repr.Wrap (WrapRepr)
-- import Noodle.Fn.ToFn (toFnX, name, args, Argument(..))

import Type.Proxy (Proxy)

import Data.Text.Format as T

import Noodle.Text.Code.Target (Target)


class ToCode (target :: Target) opts a where
    toCode :: Proxy target -> opts -> a -> String


class ToTaggedCode (target :: Target) opts a where
    toTaggedCode :: Proxy target -> opts -> a -> T.Tag


{-
nodes :: _ NDF
nodes = Proxy


fnPs :: forall a. ToCode PS a => String -> Array a -> String
fnPs name vals = fnsPs name $ toCode pureScript <$> vals


fnsPs :: String -> Array String -> String
fnsPs name vals = name <> " " <> (String.joinWith " " vals)


fnePs :: String -> String
fnePs name = name


fnJs :: forall a. ToCode JS a => String -> Array a -> String
fnJs name vals = fnsJs name $ toCode javaScript <$> vals


fnsJs :: String -> Array String -> String
fnsJs name vals = name <> "( " <> (String.joinWith ", " vals) <> " )"


fneJs :: String -> String
fneJs name = name <> "()"
-}