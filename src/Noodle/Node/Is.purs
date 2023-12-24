module Noodle.Node.Is where


import Data.Repr (class FromRepr, class ReadWriteRepr, class ToRepr)

import Noodle.Id (class HasInput)


class IsInputInNode :: Symbol → Type → Row Type → Row Type → Type → Constraint
class
  ( HasInput i din is' is
  , ReadWriteRepr repr
  , ToRepr din repr
  , FromRepr repr din
  ) <= IsInputInNode i din is' is repr


instance
  ( HasInput i din is' is
  , ReadWriteRepr repr
  , ToRepr din repr
  , FromRepr repr din
  ) => IsInputInNode i din is' is repr
