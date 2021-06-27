module Noodle.Node.Channel where


type Channel d = { accept :: (d -> Boolean) }