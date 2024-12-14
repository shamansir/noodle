module Cli.Class.CliFriendly where

import Prelude

import Noodle.Toolkit (class MarkToolkit, class HasChRepr) as Toolkit
import Noodle.Ui.Cli.Tagging.At (class At) as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel, StatusLine) as At

import Cli.Class.CliRenderer (class CliRenderer)


-- FIXME: ensure those are the only classes required in the places where it is used


class
    ( CliRenderer tk fs chrepr m
    , Toolkit.HasChRepr tk chrepr
    , Toolkit.MarkToolkit tk
    , T.At At.ChannelLabel chrepr
    , T.At At.StatusLine chrepr
    ) <= CliFriendly tk fs chrepr m


instance
    ( CliRenderer tk fs chrepr m
    , Toolkit.HasChRepr tk chrepr
    , Toolkit.MarkToolkit tk
    , T.At At.ChannelLabel chrepr
    , T.At At.StatusLine chrepr
    ) => CliFriendly tk fs chrepr m