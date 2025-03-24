module Noodle.Ui.Tagging.At where

import Type.Proxy (Proxy(..))

import Data.Text.Format (Tag)

data Loc

foreign import data StatusLine :: Loc
foreign import data Documentation :: Loc
foreign import data ChannelLabel :: Loc
foreign import data InfoBox :: Loc
foreign import data InfoNode :: Loc


class At (loc :: Loc) a where
    at :: Proxy loc -> a -> Tag


statusLine :: forall a. At StatusLine a => a -> Tag
statusLine = at (Proxy :: _ StatusLine)


channelLabel :: forall a. At ChannelLabel a => a -> Tag
channelLabel = at (Proxy :: _ ChannelLabel)


documentation :: forall a. At Documentation a => a -> Tag
documentation = at (Proxy :: _ Documentation)


infoNode :: forall a. At InfoNode a => a -> Tag -- TODO: USE in Tagging
infoNode = at (Proxy :: _ InfoNode) -- same as status line?


infoBox :: forall a. At InfoBox a => a -> Tag -- TODO: USE in Tagging
infoBox = at (Proxy :: _ InfoBox)