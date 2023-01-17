module Blessed.UI.Screen.Property where


import Prim.Row as R
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Time.Duration (Milliseconds(..))
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.Core (Attribute, property) as C
import Blessed.Core.Cursor (Cursor)
import Blessed.Core.Key (Key)
import Blessed.UI.Box.Property as Box
import Blessed.UI.Screen.Event (Event)


-- instance EncodeJson (Milliseconds) where
--     encodeJson (Milliseconds v) = encodeJson v


type PropertiesRow :: Row Type -> Row Type
type PropertiesRow r =
    ( title :: String
    -- TODO: program ?
    , smartCSR :: Boolean
    , fastCSR :: Boolean
    , useBCE :: Boolean
    , resizeTimout :: Int
    , tabSize :: Int
    , autoPadding :: Boolean
    , cursor :: Cursor
    , log :: Boolean -- could be a file path
    , dump :: Boolean -- could be a file path
    , debug :: Boolean
    , ignoreLocked :: Array Key
    , dockBorders :: Boolean
    , ignoreDockContrast :: Boolean
    , fullUnicode :: Boolean
    , sendFocus :: Boolean
    , warnings :: Boolean
    -- , input :: Stream -- TODO
    -- , output :: Stream -- TODO
    , terminal :: String -- TODO
    | Box.PropertiesRow + r
    )
type PropertiesU = PropertiesRow ()
type Properties = Record (PropertiesU)



type ScreenAttribute r = C.Attribute (PropertiesRow + r) Event


screenProperty :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> ScreenAttribute r
screenProperty = C.property



--draggable :: forall r e. Boolean -> Prop ( draggable :: Boolean | r ) e

title :: forall r. String -> ScreenAttribute ( title :: String | r)
title = screenProperty (Proxy :: _ "title")


smartCSR :: forall r. Boolean -> ScreenAttribute ( smartCSR :: Boolean | r)
smartCSR = screenProperty (Proxy :: _ "smartCSR")


fastCSR :: forall r. Boolean -> ScreenAttribute ( fastCSR :: Boolean | r)
fastCSR = screenProperty (Proxy :: _ "fastCSR")


useBCE :: forall r. Boolean -> ScreenAttribute ( useBCE :: Boolean | r)
useBCE = screenProperty (Proxy :: _ "useBCE")


-- resizeTimout :: forall r. Milliseconds -> ScreenAttribute ( resizeTimout :: Milliseconds | r)
-- resizeTimout = screenProperty (Proxy :: _ "resizeTimout")


resizeTimout :: forall r. Int -> ScreenAttribute ( resizeTimout :: Int | r)
resizeTimout = screenProperty (Proxy :: _ "resizeTimout")


tabSize :: forall r. Int -> ScreenAttribute ( tabSize :: Int | r)
tabSize = screenProperty (Proxy :: _ "tabSize")


autoPadding :: forall r. Boolean -> ScreenAttribute ( autoPadding :: Boolean | r)
autoPadding = screenProperty (Proxy :: _ "autoPadding")


cursor :: forall r. Cursor -> ScreenAttribute ( cursor :: Cursor | r)
cursor = screenProperty (Proxy :: _ "cursor")


log :: forall r. Boolean  -> ScreenAttribute ( log :: Boolean  | r)
log = screenProperty (Proxy :: _ "log")


dump :: forall r. Boolean  -> ScreenAttribute ( dump :: Boolean  | r)
dump = screenProperty (Proxy :: _ "dump")


debug :: forall r. Boolean -> ScreenAttribute ( debug :: Boolean | r)
debug = screenProperty (Proxy :: _ "debug")


ignoreLocked :: forall r. Array Key -> ScreenAttribute ( ignoreLocked :: Array Key | r)
ignoreLocked = screenProperty (Proxy :: _ "ignoreLocked")


dockBorders :: forall r. Boolean -> ScreenAttribute ( dockBorders :: Boolean | r)
dockBorders = screenProperty (Proxy :: _ "dockBorders")


ignoreDockContrast :: forall r. Boolean -> ScreenAttribute ( ignoreDockContrast :: Boolean | r)
ignoreDockContrast = screenProperty (Proxy :: _ "ignoreDockContrast")


fullUnicode :: forall r. Boolean -> ScreenAttribute ( fullUnicode :: Boolean | r)
fullUnicode = screenProperty (Proxy :: _ "fullUnicode")


sendFocus :: forall r. Boolean -> ScreenAttribute ( sendFocus :: Boolean | r)
sendFocus = screenProperty (Proxy :: _ "sendFocus")


warnings :: forall r. Boolean -> ScreenAttribute ( warnings :: Boolean | r)
warnings = screenProperty (Proxy :: _ "warnings")


terminal :: forall r. String  -> ScreenAttribute ( terminal :: String  | r)
terminal = screenProperty (Proxy :: _ "terminal")
