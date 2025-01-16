module Noodle.Repr.ChRepr
    ( class ReadChannelRepr, readChannelRepr
    , class WriteChannelRepr, writeChannelRepr
    , class ReadWriteChannelRepr
    )
    where

import Data.Maybe (Maybe)


class ReadChannelRepr repr where
    readChannelRepr :: String -> Maybe repr


class WriteChannelRepr repr where
    writeChannelRepr :: repr -> String


class    (ReadChannelRepr repr, WriteChannelRepr repr) <= ReadWriteChannelRepr repr
instance (ReadChannelRepr repr, WriteChannelRepr repr) => ReadWriteChannelRepr repr