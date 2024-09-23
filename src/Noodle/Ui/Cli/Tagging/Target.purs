module Noodle.Ui.Cli.Tagging.Target where

import Prelude
data InfoRepr = InfoRepr { shortLabel :: VShortChannelLabel, statusLine :: VStatusLine }


type VShortChannelLabel = String -- Vect 4 (Maybe SCP.CodePoint) -- ShortChannelValueLabel


type VStatusLine = String -- TODO: Tagged


type VDocs = String -- TODO: Tagged


class ShortChannelLabel a where
    shortLabel :: a -> VShortChannelLabel


class StatusLineInfo a where
    statusLine :: a -> VStatusLine -- Tagged


class Documentation a where
    docs :: a -> VDocs


instance ShortChannelLabel InfoRepr where
    shortLabel (InfoRepr info) = info.shortLabel


instance StatusLineInfo InfoRepr where
    statusLine (InfoRepr info) = info.statusLine