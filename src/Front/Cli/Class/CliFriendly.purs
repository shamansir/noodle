module Cli.Class.CliFriendly where

import Prelude

import Noodle.Toolkit (class MarkToolkit, class HasChRepr) as Toolkit
import Noodle.Ui.Cli.Tagging.At (class At) as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel, StatusLine, Documentation) as At
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ValueEncode) as Ndf

import Cli.Class.CliRenderer (class CliRenderer, class CliRawRenderer, class CliEditor)


-- FIXME: ensure those are the only classes required in the places where it is used


class
    ( CliRenderer tk fs chrepr m
    , CliRawRenderer tk fs chrepr m
    , CliEditor tk chrepr
    , Toolkit.HasChRepr tk chrepr
    , Toolkit.MarkToolkit tk
    , T.At At.ChannelLabel chrepr
    , T.At At.StatusLine chrepr
    , T.At At.Documentation chrepr
    , Ndf.ValueEncode chrepr
    ) <= CliFriendly tk fs chrepr m


instance
    ( CliRenderer tk fs chrepr m
    , CliRawRenderer tk fs chrepr m
    , CliEditor tk chrepr
    , Toolkit.HasChRepr tk chrepr
    , Toolkit.MarkToolkit tk
    , T.At At.ChannelLabel chrepr
    , T.At At.StatusLine chrepr
    , T.At At.Documentation chrepr
    , Ndf.ValueEncode chrepr
    ) => CliFriendly tk fs chrepr m