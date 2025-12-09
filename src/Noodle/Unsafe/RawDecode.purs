module Noodle.Unsafe.RawDecode where

import Prelude
-- import Debug as Debug

import Data.Maybe (Maybe, fromMaybe)

import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..)) as Ndf
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr, toRepr, toDefault) as Ndf


rawDecode :: forall chrepr r. Ndf.ParseableRepr chrepr => { tag :: String, value :: Maybe String | r } -> chrepr
rawDecode { tag, value } =
    -- Debug.spy "value" value
    value
        >>= (Ndf.EncodedValue >>> Ndf.toRepr (Ndf.EncodedType tag))
        # fromMaybe (Ndf.toDefault $ Ndf.EncodedType tag)
        -- # Debug.spy "decoded"