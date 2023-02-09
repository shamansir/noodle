module Blessed.UI.DataDisplay.Table.Option where


import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.EndStyle (EndStyleOption)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, Table)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( rows :: Array (Array String)
    , pad :: Int
    , noCellBorders :: Boolean
    , fillCellBorders :: Boolean
    , style_header :: Array (EndStyleOption ())
    , style_cell :: Array (EndStyleOption ())
    | r
    )
type Options = Record (OptionsRow ())


type TableAttribute subj id r state e = C.Attribute subj id (List.OptionsRow + OptionsRow + r) state e


tableOption
    :: forall subj id a r r' sym state e
     . Respresents Table subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> TableAttribute subj id r state e
tableOption = C.option


rows
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Table subj id
    => Array (Array String) -> TableAttribute subj id ( rows :: Array (Array String) | r ) state e
rows = tableOption (Proxy :: _ "rows")


pad
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Table subj id
    => Int -> TableAttribute subj id ( pad :: Int | r ) state e
pad = tableOption (Proxy :: _ "pad")


noCellBorders
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Table subj id
    => Boolean -> TableAttribute subj id ( noCellBorders :: Boolean | r ) state e
noCellBorders = tableOption (Proxy :: _ "noCellBorders")


fillCellBorders
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Table subj id
    => Boolean -> TableAttribute subj id ( fillCellBorders :: Boolean | r ) state e
fillCellBorders = tableOption (Proxy :: _ "fillCellBorders")


style_header
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Table subj id
    => Array (EndStyleOption ()) -> TableAttribute subj id ( style_header :: Array (EndStyleOption ()) | r ) state e
style_header = tableOption (Proxy :: _ "style_header")


style_cell
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Respresents Table subj id
    => Array (EndStyleOption ()) -> TableAttribute subj id ( style_cell :: Array (EndStyleOption ()) | r ) state e
style_cell = tableOption (Proxy :: _ "style_cell")
