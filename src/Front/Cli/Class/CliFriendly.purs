module Cli.Class.CliFriendly where

import Prelude

import Noodle.Repr (class HasFallback)
import Noodle.Wiring (class Wiring)
import Noodle.Toolkit (class MapFamiliesImpl, class MarkToolkit) as Toolkit
import Noodle.Ui.Cli.Tagging.At (class At) as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel) as At
import Noodle.Ui.Cli.Palette.Mark (class Mark)

import Cli.Class.CliRenderer (class CliRenderer)


-- FIXME: ensure those are the only classes required in the places where it is used

class
    ( Wiring m
    , CliRenderer tk fs repr m
    , HasFallback repr
    , Toolkit.MapFamiliesImpl repr m fs
    , Toolkit.MarkToolkit tk, Mark repr, T.At At.ChannelLabel repr
    ) <= CliFriendly tk fs repr m


instance
    ( Wiring m
    , CliRenderer tk fs repr m
    , HasFallback repr
    , Toolkit.MapFamiliesImpl repr m fs
    , Toolkit.MarkToolkit tk, Mark repr, T.At At.ChannelLabel repr
    ) => CliFriendly tk fs repr m