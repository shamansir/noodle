module Cli.Class.CliFriendly where

import Prelude

import Noodle.Repr (class HasFallback)
import Noodle.Wiring (class Wiring)
import Noodle.Toolkit (class HoldsFamilies, class MarkToolkit, class HasRepr) as Toolkit
import Noodle.Ui.Cli.Tagging.At (class At) as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel) as At

import Cli.Class.CliRenderer (class CliRenderer)


-- FIXME: ensure those are the only classes required in the places where it is used


class
    ( CliRenderer tk fs repr m
    , Toolkit.HasRepr tk repr
    , Toolkit.MarkToolkit tk
    , T.At At.ChannelLabel repr
    ) <= CliFriendly tk fs repr m


instance
    ( CliRenderer tk fs repr m
    , Toolkit.HasRepr tk repr
    , Toolkit.MarkToolkit tk
    , T.At At.ChannelLabel repr
    ) => CliFriendly tk fs repr m