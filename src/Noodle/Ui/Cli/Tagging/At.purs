module Noodle.Ui.Cli.Tagging.At where

import Type.Proxy (Proxy(..))

import Data.Text.Format (Tag)

data Loc

foreign import data StatusLine :: Loc
foreign import data Documentation :: Loc
foreign import data ChannelLabel :: Loc


class At (loc :: Loc) a where
    at :: Proxy loc -> a -> Tag


statusLine :: forall a. At StatusLine a => a -> Tag
statusLine = at (Proxy :: _ StatusLine)


channelLabel :: forall a. At ChannelLabel a => a -> Tag
channelLabel = at (Proxy :: _ ChannelLabel)


documentation :: forall a. At Documentation a => a -> Tag
documentation = at (Proxy :: _ Documentation)