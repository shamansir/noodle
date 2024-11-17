module Hydra.Repr.Markers where



_argSep :: String
_argSep = ";"

_argsEnd :: String
_argsEnd = ";" -- FIXME: try different values here and run tests, seems parsing functions works only with this option and also doesn't consume all the output

_texSep :: String
_texSep = " % "

_texsEnd :: String
_texsEnd = " %"

_unparsedFnStart :: String
_unparsedFnStart = "¤■"

_unparsedFnTerminals :: Array Char
_unparsedFnTerminals = [ '¤', '■' ]

_unparsedFnEnd :: String
_unparsedFnEnd = "■¤"

_jsexprStart :: String
_jsexprStart = "```"

_jsexprTerminals :: Array Char
_jsexprTerminals = [ '`' ]

_jsexprEnd :: String
_jsexprEnd = "```"

_glslMarker :: String
_glslMarker = "<GLSL>"

_glslStart :: String
_glslStart = "¤¤¤¤■"

_glslTerminals :: Array Char
_glslTerminals = [ '¤', '■' ]

_glslEnd :: String
_glslEnd = "■¤¤¤¤"

_arrEmpty :: String
_arrEmpty = "%%%%"

_arrStart :: String
_arrStart = "%% "

_arrEnd :: String
_arrEnd = " %%"

_arrSep :: String
_arrSep = " <> "