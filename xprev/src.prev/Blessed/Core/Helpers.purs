module Blessed.Core.Helpers where

import Prelude


-- TODO:

-- All helpers reside on blessed.helpers or blessed.

-- merge(a, b) - Merge objects a and b into object a.
-- asort(obj) - Sort array alphabetically by name prop.
-- hsort(obj) - Sort array numerically by index prop.
-- findFile(start, target) - Find a file at start directory with name target.
-- escape(text) - Escape content's tags to be passed into el.setContent(). Example: box.setContent('escaped tag: ' + blessed.escape('{bold}{/bold}'));
-- parseTags(text) - Parse tags into SGR escape codes.
-- generateTags(style, text) - Generate text tags based on style object.
-- attrToBinary(style, element) - Convert style attributes to binary format.
-- stripTags(text) - Strip text of tags and SGR sequences.
-- cleanTags(text) - Strip text of tags, SGR escape code, and leading/trailing whitespace.
-- dropUnicode(text) - Drop text of any >U+FFFF characters.