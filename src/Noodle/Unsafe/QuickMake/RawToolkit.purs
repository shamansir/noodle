module Noodle.Unsafe.QuickMake.RawToolkit where

import Prelude

import Data.Maybe (Maybe)

import Noodle.Id (unsafeFamilyR) as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (registerRaw) as Toolkit
import Noodle.Unsafe.QuickMake.RawFamily (qmake) as Family
import Noodle.Repr.HasFallback (fallback) as Repr
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Raw.Fn.Process (Process) as Raw
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr) as Ndf
import Noodle.Unsafe.RawDecode (rawDecode)


qregister
    :: forall tk families state chrepr m
     . HasFallback state
    => Ndf.ParseableRepr chrepr
    => String
    -> Array { name :: String, tag :: String , value :: Maybe String }
    -> Array { name :: String, tag :: String , value :: Maybe String }
    -> Raw.Process state chrepr m
    -> Toolkit tk families state chrepr m
    -> Toolkit tk families state chrepr m
qregister family inlets outlets =
    Toolkit.registerRaw
    <<< Family.qmake
        (Id.unsafeFamilyR family)
        Repr.fallback -- FIXME
        rawDecode
        { inlets, outlets }