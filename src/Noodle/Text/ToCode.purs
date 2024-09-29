module Noodle.Text.ToCode where

import Prelude hiding (show)
import Prelude (show) as Core

-- import Toolkit.Hydra.Types
-- import Toolkit.Hydra.Repr.Wrap (WrapRepr)
-- import Noodle.Fn.ToFn (toFnX, name, args, Argument(..))

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))


import Data.Text.Format as T


data Target


-- could they be split into different files?
-- TODO: NDF format is not bound to Hydra, rather to Noodle Engine, move it to external module
foreign import data JS :: Target
foreign import data JS_DISPLAY :: Target -- TODO: replace with `ToTaggedCode JS` later
foreign import data PS :: Target
foreign import data NDF :: Target


class ToCode (target :: Target) opts a where
    toCode :: Proxy target -> opts -> a -> String


class ToTaggedCode (target :: Target) opts a where
    toTaggedCode :: Proxy target -> opts -> a -> T.Tag


pureScript :: _ PS
pureScript = Proxy


javaScript :: _ JS
javaScript = Proxy


javaScriptToDisplay :: _ JS_DISPLAY
javaScriptToDisplay = Proxy


ndf :: _ NDF
ndf = Proxy


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
