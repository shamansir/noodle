module Blessed.UI.Base.Screen.Option where


import Prim.Row as R
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Time.Duration (Milliseconds(..))
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Core.Cursor (Cursor)
import Blessed.Core.Key (Key)
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Base.Screen.Event (Event)


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



type ScreenAttribute r e = C.Attribute (OptionsRow + r) e


screenOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ScreenAttribute r e
screenOption = C.option


title :: forall r e. String -> ScreenAttribute ( title :: String | r ) e
title = screenOption (Proxy :: _ "title")


smartCSR :: forall r e. Boolean -> ScreenAttribute ( smartCSR :: Boolean | r ) e
smartCSR = screenOption (Proxy :: _ "smartCSR")


fastCSR :: forall r e. Boolean -> ScreenAttribute ( fastCSR :: Boolean | r ) e
fastCSR = screenOption (Proxy :: _ "fastCSR")


useBCE :: forall r e. Boolean -> ScreenAttribute ( useBCE :: Boolean | r ) e
useBCE = screenOption (Proxy :: _ "useBCE")


-- resizeTimout :: forall r e. Milliseconds -> ScreenAttribute ( resizeTimout :: Milliseconds | r ) e
-- resizeTimout = screenOption (Proxy :: _ "resizeTimout")


resizeTimout :: forall r e. Int -> ScreenAttribute ( resizeTimout :: Int | r ) e
resizeTimout = screenOption (Proxy :: _ "resizeTimout")


tabSize :: forall r e. Int -> ScreenAttribute ( tabSize :: Int | r ) e
tabSize = screenOption (Proxy :: _ "tabSize")


autoPadding :: forall r e. Boolean -> ScreenAttribute ( autoPadding :: Boolean | r ) e
autoPadding = screenOption (Proxy :: _ "autoPadding")


cursor :: forall r e. Cursor -> ScreenAttribute ( cursor :: Cursor | r ) e
cursor = screenOption (Proxy :: _ "cursor")


log :: forall r e. Boolean  -> ScreenAttribute ( log :: Boolean  | r ) e
log = screenOption (Proxy :: _ "log")


dump :: forall r e. Boolean  -> ScreenAttribute ( dump :: Boolean  | r ) e
dump = screenOption (Proxy :: _ "dump")


debug :: forall r e. Boolean -> ScreenAttribute ( debug :: Boolean | r ) e
debug = screenOption (Proxy :: _ "debug")


ignoreLocked :: forall r e. Array Key -> ScreenAttribute ( ignoreLocked :: Array Key | r ) e
ignoreLocked = screenOption (Proxy :: _ "ignoreLocked")


dockBorders :: forall r e. Boolean -> ScreenAttribute ( dockBorders :: Boolean | r ) e
dockBorders = screenOption (Proxy :: _ "dockBorders")


ignoreDockContrast :: forall r e. Boolean -> ScreenAttribute ( ignoreDockContrast :: Boolean | r ) e
ignoreDockContrast = screenOption (Proxy :: _ "ignoreDockContrast")


fullUnicode :: forall r e. Boolean -> ScreenAttribute ( fullUnicode :: Boolean | r ) e
fullUnicode = screenOption (Proxy :: _ "fullUnicode")


sendFocus :: forall r e. Boolean -> ScreenAttribute ( sendFocus :: Boolean | r ) e
sendFocus = screenOption (Proxy :: _ "sendFocus")


warnings :: forall r e. Boolean -> ScreenAttribute ( warnings :: Boolean | r ) e
warnings = screenOption (Proxy :: _ "warnings")


terminal :: forall r e. String  -> ScreenAttribute ( terminal :: String  | r ) e
terminal = screenOption (Proxy :: _ "terminal")
