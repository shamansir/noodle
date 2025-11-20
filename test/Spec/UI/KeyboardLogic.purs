module Test.Spec.UI.KeyboardLogic where

import Prelude

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)


import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn, fail)

import Noodle.Id (familyR, family, FamilyR(..), toolkitR) as Id
import Noodle.Raw.Node (shape) as RawNode
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Raw.FromToRec as RR
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, register) as Toolkit
import Noodle.Fn.Shape.Temperament (Temperament(..)) as Temp

import Example.Toolkit.Minimal.ChRepr (MinimalVRepr(..))
import Example.Toolkit.Minimal.StRepr (MinimalStRepr(..))
import Example.Toolkit.Minimal.Node.Concat as Concat
import Example.Toolkit.Minimal.Node.Sum as Sum
import Example.Toolkit.Minimal.Toolkit (Toolkit, toolkit, MINIMAL,  minimalTk) as Minimal

import Web.Components.AppScreen.KeyboardLogic as KL

-- type MinimalTk fs m = Toolkit My.MINIMAL fs MinimalStRepr MinimalVRepr m


spec :: Spec Unit
spec = do

    describe "keyboard logic" $ do

        it "properly converts index to a letter" $ do
            KL.toAxis (KL.InletIndex 0) `shouldEqual` (KL.Number 0)
            KL.toAxis (KL.InletIndex 10) `shouldEqual` (KL.Letter "a")
            KL.toAxis (KL.InletIndex 15) `shouldEqual` (KL.Letter "f")
            KL.toAxis (KL.InletIndex 20) `shouldEqual` (KL.Letter "k")
            KL.toAxis (KL.InletIndex 35) `shouldEqual` (KL.Letter "z")