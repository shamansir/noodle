module Noodle.Text.NetworkFile.Command where

import Prelude


data Command
    = Header String String
    | MakeNode String Int Int String
    | Connect String Int String Int