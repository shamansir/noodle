module Noodle.Text.Code.Target where

import Type.Proxy (Proxy(..))

data Target


-- could they be split into different files?
foreign import data JS :: Target
foreign import data JS_DISPLAY :: Target -- TODO: replace with `ToTaggedCode JS` later
foreign import data PS :: Target
foreign import data NDF :: Target


pureScript :: _ PS
pureScript = Proxy


javaScript :: _ JS
javaScript = Proxy


javaScriptToDisplay :: _ JS_DISPLAY
javaScriptToDisplay = Proxy


ndf :: _ NDF
ndf = Proxy
