module Blessed.UI.Lists.ListTable.Option where

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.EndStyle (EndStyleOption)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, ListTable)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( rows :: Array (Array String)
    , pad :: Int
    , noCellBorders :: Boolean
    , style_header :: Array (EndStyleOption ())
    , style_cell :: Array (EndStyleOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ListTableAttribute subj id r state e = C.Attribute subj id (List.OptionsRow + OptionsRow + r) state e


ltOption
    :: forall subj id a r r' sym state e
     . Respresents ListTable subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ListTableAttribute subj id r state e
ltOption = C.option


rows
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ListTable subj id
    => Array (Array String) -> ListTableAttribute subj id ( rows :: Array (Array String) | r ) state e
rows = ltOption (Proxy :: _ "rows")


pad
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ListTable subj id
    => Int -> ListTableAttribute subj id ( pad :: Int | r ) state e
pad = ltOption (Proxy :: _ "pad")


noCellBorders
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ListTable subj id
    => Boolean -> ListTableAttribute subj id ( noCellBorders :: Boolean | r ) state e
noCellBorders = ltOption (Proxy :: _ "noCellBorders")


style_header
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ListTable subj id
    => Array (EndStyleOption ()) -> ListTableAttribute subj id ( style_header :: Array (EndStyleOption ()) | r ) state e
style_header = ltOption (Proxy :: _ "style_header")


style_cell
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents ListTable subj id
    => Array (EndStyleOption ()) -> ListTableAttribute subj id ( style_cell :: Array (EndStyleOption ()) | r ) state e
style_cell = ltOption (Proxy :: _ "style_cell")
