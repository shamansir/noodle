module MSetBins where

import Prelude

_in_a = Fn.Input :: _ "a"
_in_numBins = Fn.Input :: _ "numBins"

_out_out = Fn.Output :: _ "out"

type Family m = -- {-> audio <-}
    Family.Def Unit
        ( a :: Audio, numBins :: Value )
        ( out :: Unit )
        m

fml :: forall m. Family m
fml = -- {-> audio <-}
    Family.def
        unit
        { a : ?a_default, numBins : 4 }
        { out : ?out_default }
        $ fml.make $ do
            a <- P.receive _in_a
            numBins <- P.receive _in_numBins
            -- SetBins _in_a _in_numBins
            P.send _out_out ?out_out
