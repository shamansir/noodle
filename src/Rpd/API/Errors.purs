module Rpd.API.Errors where


import Prelude


newtype RpdError = RpdError String


instance showRpdError :: Show RpdError where show (RpdError err) = err
derive instance eqRpdError :: Eq RpdError


