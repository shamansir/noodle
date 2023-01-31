module Blessed.UI.DataDisplay.Table.Option where


import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, Table)
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Lists.List.Event (Event)
import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( rows :: Array (Array String)
    , pad :: Int
    , noCellBorders :: Boolean
    , fillCellBorders :: Boolean
    , style_header :: Array (FgBgOption ())
    , style_cell :: Array (FgBgOption ())
    | r
    )
type Options = Record (OptionsRow ())


type TableAttribute subj id r e = C.Attribute subj id (List.OptionsRow + OptionsRow + r) e


tableOption
    :: forall subj id a r r' sym e
     . Respresents Table subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> TableAttribute subj id r e
tableOption = C.option


rows
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Table subj id
    => Array (Array String) -> TableAttribute subj id ( rows :: Array (Array String) | r ) e
rows = tableOption (Proxy :: _ "rows")


pad
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Table subj id
    => Int -> TableAttribute subj id ( pad :: Int | r ) e
pad = tableOption (Proxy :: _ "pad")


noCellBorders
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Table subj id
    => Boolean -> TableAttribute subj id ( noCellBorders :: Boolean | r ) e
noCellBorders = tableOption (Proxy :: _ "noCellBorders")


fillCellBorders
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Table subj id
    => Boolean -> TableAttribute subj id ( fillCellBorders :: Boolean | r ) e
fillCellBorders = tableOption (Proxy :: _ "fillCellBorders")


style_header
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Table subj id
    => Array (FgBgOption ()) -> TableAttribute subj id ( style_header :: Array (FgBgOption ()) | r ) e
style_header = tableOption (Proxy :: _ "style_header")


style_cell
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Table subj id
    => Array (FgBgOption ()) -> TableAttribute subj id ( style_cell :: Array (FgBgOption ()) | r ) e
style_cell = tableOption (Proxy :: _ "style_cell")
