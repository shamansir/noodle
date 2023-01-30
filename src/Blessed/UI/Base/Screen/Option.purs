module Blessed.UI.Base.Screen.Option where


import Prim.Row as R
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Data.Symbol (class IsSymbol)

import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Core.Cursor (Cursor)
import Blessed.Core.Key (Key)
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.Internal.BlessedSubj (Subject, Screen)
import Blessed.Internal.NodeKey (class Respresents)


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



type ScreenAttribute subj id r e = C.Attribute subj id (OptionsRow + r) e


screenOption
    :: forall subj id a r r' sym e
     . Respresents Screen subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ScreenAttribute subj id r e
screenOption = C.option


title
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => String -> ScreenAttribute subj id ( title :: String | r ) e
title = screenOption (Proxy :: _ "title")


smartCSR
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( smartCSR :: Boolean | r ) e
smartCSR = screenOption (Proxy :: _ "smartCSR")


fastCSR
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( fastCSR :: Boolean | r ) e
fastCSR = screenOption (Proxy :: _ "fastCSR")


useBCE
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( useBCE :: Boolean | r ) e
useBCE = screenOption (Proxy :: _ "useBCE")


{- resizeTimout
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Milliseconds -> ScreenAttribute subj id ( resizeTimout :: Milliseconds | r ) e
resizeTimout = screenOption (Proxy :: _ "resizeTimout")
-}


resizeTimout
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Int -> ScreenAttribute subj id ( resizeTimout :: Int | r ) e
resizeTimout = screenOption (Proxy :: _ "resizeTimout")


tabSize
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Int -> ScreenAttribute subj id ( tabSize :: Int | r ) e
tabSize = screenOption (Proxy :: _ "tabSize")


autoPadding
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( autoPadding :: Boolean | r ) e
autoPadding = screenOption (Proxy :: _ "autoPadding")


cursor
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Cursor -> ScreenAttribute subj id ( cursor :: Cursor | r ) e
cursor = screenOption (Proxy :: _ "cursor")


log
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean  -> ScreenAttribute subj id ( log :: Boolean  | r ) e
log = screenOption (Proxy :: _ "log")


dump
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean  -> ScreenAttribute subj id ( dump :: Boolean  | r ) e
dump = screenOption (Proxy :: _ "dump")


debug
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( debug :: Boolean | r ) e
debug = screenOption (Proxy :: _ "debug")


ignoreLocked
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Array Key -> ScreenAttribute subj id ( ignoreLocked :: Array Key | r ) e
ignoreLocked = screenOption (Proxy :: _ "ignoreLocked")


dockBorders
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( dockBorders :: Boolean | r ) e
dockBorders = screenOption (Proxy :: _ "dockBorders")


ignoreDockContrast
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( ignoreDockContrast :: Boolean | r ) e
ignoreDockContrast = screenOption (Proxy :: _ "ignoreDockContrast")


fullUnicode
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( fullUnicode :: Boolean | r ) e
fullUnicode = screenOption (Proxy :: _ "fullUnicode")


sendFocus
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( sendFocus :: Boolean | r ) e
sendFocus = screenOption (Proxy :: _ "sendFocus")


warnings
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( warnings :: Boolean | r ) e
warnings = screenOption (Proxy :: _ "warnings")


terminal
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Screen subj id
    => String  -> ScreenAttribute subj id ( terminal :: String  | r ) e
terminal = screenOption (Proxy :: _ "terminal")
