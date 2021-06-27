module Noodle.Channel where


type Channel d = { accept :: (d -> Boolean) }