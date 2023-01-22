module Blessed.UI.Special.Terminal.Option where

import Prelude



import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)
import Blessed.Core.Cursor (Cursor)
import Blessed.Core.Terminal (Terminal)

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.Lists.List.Event (Event)
import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( handler :: Unit -- TODO
    , shell :: String
    , args :: Array Json
    , cursor :: Cursor
    , terminal :: Terminal
    | r
    )
type Options = Record (OptionsRow ())


type TerminalAttribute r = C.Attribute (List.OptionsRow + OptionsRow + r) Event


terminalOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> TerminalAttribute r
terminalOption = C.option


handler :: forall r. Unit -> TerminalAttribute ( handler :: Unit | r )
handler = terminalOption (Proxy :: _ "handler")


shell :: forall r. String -> TerminalAttribute ( shell :: String | r )
shell = terminalOption (Proxy :: _ "shell")


args :: forall r. Array Json -> TerminalAttribute ( args :: Array Json | r )
args = terminalOption (Proxy :: _ "args")


cursor :: forall r. Cursor -> TerminalAttribute ( cursor :: Cursor | r )
cursor = terminalOption (Proxy :: _ "cursor")


terminal :: forall r. Terminal -> TerminalAttribute ( terminal :: Terminal | r )
terminal = terminalOption (Proxy :: _ "terminal")
