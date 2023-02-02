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



type ScreenAttribute subj id r state e = C.Attribute subj id (OptionsRow + r) state e


screenOption
    :: forall subj id a r r' sym state e
     . Respresents Screen subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ScreenAttribute subj id r state e
screenOption = C.option


title
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => String -> ScreenAttribute subj id ( title :: String | r ) state e
title = screenOption (Proxy :: _ "title")


smartCSR
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( smartCSR :: Boolean | r ) state e
smartCSR = screenOption (Proxy :: _ "smartCSR")


fastCSR
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( fastCSR :: Boolean | r ) state e
fastCSR = screenOption (Proxy :: _ "fastCSR")


useBCE
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( useBCE :: Boolean | r ) state e
useBCE = screenOption (Proxy :: _ "useBCE")


{- resizeTimout
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Milliseconds -> ScreenAttribute subj id ( resizeTimout :: Milliseconds | r ) state e
resizeTimout = screenOption (Proxy :: _ "resizeTimout")
-}


resizeTimout
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Int -> ScreenAttribute subj id ( resizeTimout :: Int | r ) state e
resizeTimout = screenOption (Proxy :: _ "resizeTimout")


tabSize
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Int -> ScreenAttribute subj id ( tabSize :: Int | r ) state e
tabSize = screenOption (Proxy :: _ "tabSize")


autoPadding
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( autoPadding :: Boolean | r ) state e
autoPadding = screenOption (Proxy :: _ "autoPadding")


cursor
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Cursor -> ScreenAttribute subj id ( cursor :: Cursor | r ) state e
cursor = screenOption (Proxy :: _ "cursor")


log
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean  -> ScreenAttribute subj id ( log :: Boolean  | r ) state e
log = screenOption (Proxy :: _ "log")


dump
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean  -> ScreenAttribute subj id ( dump :: Boolean  | r ) state e
dump = screenOption (Proxy :: _ "dump")


debug
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( debug :: Boolean | r ) state e
debug = screenOption (Proxy :: _ "debug")


ignoreLocked
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Array Key -> ScreenAttribute subj id ( ignoreLocked :: Array Key | r ) state e
ignoreLocked = screenOption (Proxy :: _ "ignoreLocked")


dockBorders
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( dockBorders :: Boolean | r ) state e
dockBorders = screenOption (Proxy :: _ "dockBorders")


ignoreDockContrast
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( ignoreDockContrast :: Boolean | r ) state e
ignoreDockContrast = screenOption (Proxy :: _ "ignoreDockContrast")


fullUnicode
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( fullUnicode :: Boolean | r ) state e
fullUnicode = screenOption (Proxy :: _ "fullUnicode")


sendFocus
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( sendFocus :: Boolean | r ) state e
sendFocus = screenOption (Proxy :: _ "sendFocus")


warnings
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => Boolean -> ScreenAttribute subj id ( warnings :: Boolean | r ) state e
warnings = screenOption (Proxy :: _ "warnings")


terminal
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Screen subj id
    => String  -> ScreenAttribute subj id ( terminal :: String  | r ) state e
terminal = screenOption (Proxy :: _ "terminal")
