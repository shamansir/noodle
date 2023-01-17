module Blessed.UI.Screen.Option where


import Prim.Row as R
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Time.Duration (Milliseconds(..))
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Core.Cursor (Cursor)
import Blessed.Core.Key (Key)
import Blessed.UI.Box.Option as Box
import Blessed.UI.Screen.Event (Event)


-- instance EncodeJson (Milliseconds) where
--     encodeJson (Milliseconds v) = encodeJson v


type OptionsRow :: Row Type -> Row Type
type OptionsRow r =
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
    | Box.OptionsRow + r
    )
type OptionsU = OptionsRow ()
type Options = Record (OptionsU)



type ScreenAttribute r = C.Attribute (OptionsRow + r) Event


screenOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ScreenAttribute r
screenOption = C.option


title :: forall r. String -> ScreenAttribute ( title :: String | r)
title = screenOption (Proxy :: _ "title")


smartCSR :: forall r. Boolean -> ScreenAttribute ( smartCSR :: Boolean | r)
smartCSR = screenOption (Proxy :: _ "smartCSR")


fastCSR :: forall r. Boolean -> ScreenAttribute ( fastCSR :: Boolean | r)
fastCSR = screenOption (Proxy :: _ "fastCSR")


useBCE :: forall r. Boolean -> ScreenAttribute ( useBCE :: Boolean | r)
useBCE = screenOption (Proxy :: _ "useBCE")


-- resizeTimout :: forall r. Milliseconds -> ScreenAttribute ( resizeTimout :: Milliseconds | r)
-- resizeTimout = screenOption (Proxy :: _ "resizeTimout")


resizeTimout :: forall r. Int -> ScreenAttribute ( resizeTimout :: Int | r)
resizeTimout = screenOption (Proxy :: _ "resizeTimout")


tabSize :: forall r. Int -> ScreenAttribute ( tabSize :: Int | r)
tabSize = screenOption (Proxy :: _ "tabSize")


autoPadding :: forall r. Boolean -> ScreenAttribute ( autoPadding :: Boolean | r)
autoPadding = screenOption (Proxy :: _ "autoPadding")


cursor :: forall r. Cursor -> ScreenAttribute ( cursor :: Cursor | r)
cursor = screenOption (Proxy :: _ "cursor")


log :: forall r. Boolean  -> ScreenAttribute ( log :: Boolean  | r)
log = screenOption (Proxy :: _ "log")


dump :: forall r. Boolean  -> ScreenAttribute ( dump :: Boolean  | r)
dump = screenOption (Proxy :: _ "dump")


debug :: forall r. Boolean -> ScreenAttribute ( debug :: Boolean | r)
debug = screenOption (Proxy :: _ "debug")


ignoreLocked :: forall r. Array Key -> ScreenAttribute ( ignoreLocked :: Array Key | r)
ignoreLocked = screenOption (Proxy :: _ "ignoreLocked")


dockBorders :: forall r. Boolean -> ScreenAttribute ( dockBorders :: Boolean | r)
dockBorders = screenOption (Proxy :: _ "dockBorders")


ignoreDockContrast :: forall r. Boolean -> ScreenAttribute ( ignoreDockContrast :: Boolean | r)
ignoreDockContrast = screenOption (Proxy :: _ "ignoreDockContrast")


fullUnicode :: forall r. Boolean -> ScreenAttribute ( fullUnicode :: Boolean | r)
fullUnicode = screenOption (Proxy :: _ "fullUnicode")


sendFocus :: forall r. Boolean -> ScreenAttribute ( sendFocus :: Boolean | r)
sendFocus = screenOption (Proxy :: _ "sendFocus")


warnings :: forall r. Boolean -> ScreenAttribute ( warnings :: Boolean | r)
warnings = screenOption (Proxy :: _ "warnings")


terminal :: forall r. String  -> ScreenAttribute ( terminal :: String  | r)
terminal = screenOption (Proxy :: _ "terminal")
