module Noodle.Text.NetworkFile.Command where

import Prelude


data Command
    = Header String String
    | MakeNode String Int Int String
    | Connect String Int String Int
    | Send String Int String -- FIXME: not logged to state yet + when to log?