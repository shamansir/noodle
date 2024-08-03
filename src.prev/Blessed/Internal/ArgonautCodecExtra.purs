module Blessed.Internal.ArgonautCodecExtra where

import Prelude

import Data.Argonaut.Decode.Error as ADE
import Data.Codec.Argonaut (JsonDecodeError(..)) as CA


convertJsonError :: ADE.JsonDecodeError -> CA.JsonDecodeError
convertJsonError = case _ of
    ADE.TypeMismatch s -> CA.TypeMismatch s
    ADE.UnexpectedValue json -> CA.UnexpectedValue json
    ADE.AtIndex n jde -> CA.AtIndex n $ convertJsonError jde
    ADE.AtKey n jde -> CA.AtKey n $ convertJsonError jde
    ADE.Named n jde -> CA.Named n $ convertJsonError jde
    ADE.MissingValue -> CA.MissingValue


convertJsonError' :: CA.JsonDecodeError -> ADE.JsonDecodeError
convertJsonError' = case _ of
    CA.TypeMismatch s -> ADE.TypeMismatch s
    CA.UnexpectedValue json -> ADE.UnexpectedValue json
    CA.AtIndex n jde -> ADE.AtIndex n $ convertJsonError' jde
    CA.AtKey n jde -> ADE.AtKey n $ convertJsonError' jde
    CA.Named n jde -> ADE.Named n $ convertJsonError' jde
    CA.MissingValue -> ADE.MissingValue