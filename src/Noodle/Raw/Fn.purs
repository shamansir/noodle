module Noodle.Raw.Fn where


import Noodle.Id (FnName)

import Noodle.Raw.Fn.Process (Process) as Raw


data Fn state repr (m :: Type -> Type) = Fn FnName (Raw.Process state repr m) -- TODO: move to separate module


make :: forall state repr m. FnName -> Raw.Process state repr m -> Fn state repr m
make = Fn
