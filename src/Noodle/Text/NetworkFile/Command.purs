module Noodle.Text.NetworkFile.Command where


data Command
    = Header String String
    | MakeNode String Int Int String
    | Connect String Int String Int
    | Send String Int String -- FIXME: not logged to state yet + when to log?
    | SendO String Int String
    | Connect_ String String String String
    | Send_ String String String
    | SendO_ String String String