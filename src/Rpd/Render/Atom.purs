module Rpd.Render.Atom where


class Atom x where
    labelOf :: x -> String
    uniqueIdOf :: x -> String
    debugInfoOf :: x -> String
