module Blessed.UI.Lists.FileManager.Option where

import Prelude

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, FileManager)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( cwd :: String
    | r
    )
type Options = Record (OptionsRow ())


type FileManagerAttribute subj id r e = C.Attribute subj id (List.OptionsRow + OptionsRow + r) e


fmOption
    :: forall subj id a r r' sym e
     . Respresents FileManager subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> FileManagerAttribute subj id r e
fmOption = C.option


cwd
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents FileManager subj id
    => Color -> FileManagerAttribute subj id ( cwd :: String | r ) e
cwd = fmOption (Proxy :: _ "cwd")
