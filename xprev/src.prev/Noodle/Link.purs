module Noodle.Link where


import Effect.Ref (Ref)


newtype Link = Link (Ref Boolean)