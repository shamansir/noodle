module Noodle.Text.ToolkitGen where

import Prelude

import Data.Array as Array
import Data.String as String
import Data.Maybe (Maybe(..), fromMaybe)

import Noodle.Text.QuickDef as QD

wrap :: forall a. String → String → (a → String) → String → Array a → String
wrap start end fn sep items =
    if Array.length items == 0 then start <> " " <> end
    else start <> " " <> String.joinWith sep (fn <$> items) <> " " <> end


inBrackets :: forall a. (a → String) → String → Array a → String
inBrackets =
    wrap "(" ")"


inCBraces :: forall a. (a → String) → String → Array a → String
inCBraces =
    wrap "{" "}"


i = "    "
i2 = i <> i
i3 = i2 <> i
i4 = i3 <> i


genTypeDefs :: String -> Array QD.FN -> String
genTypeDefs tkName fns =
    "type Toolkit m\n" <> i <> "= Toolkit Unit\n"
        <> i2 <> inBrackets genFnTypeDef ("\n" <> i2 <> ", ") fns


genToolkitDef :: String -> Array QD.FN -> String
genToolkitDef tkName fns =
    "Toolkit.from \"" <> tkName <> "\"\n" <> i <>
        inCBraces genFnToolkitDef ("\n" <> i <> ", ") fns


{-
    """
    Toolkit.from "test"
        { foo :
            Family.def
                unit
                { foo : "aaa", bar : "bbb", c : 32 }
                { out : false }
                $ Fn.make "foo" $ pure unit"
        ,
    }
    """



-}


genFnTypeDef :: QD.FN -> String
genFnTypeDef (QD.FN fn) =
    fn.family <> " :: -- {-> " <> fn.tag <> " <-} \n"
        <> i3 <> "Family.Def Unit \n"
        <> i4 <> (inBrackets genArgToolkitDef ", " fn.args) <> "\n"
        <> i4 <> "( out :: " <> fn.returns <> " )\n"
        <> i4 <> "m"


genFnToolkitDef :: QD.FN -> String
genFnToolkitDef (QD.FN fn) =
    fn.family <> " : -- {-> " <> fn.tag <> " <-} \n"
        <> i2 <> "Family.def\n"
        <> i3 <> "unit\n"
        <> i3 <> (inCBraces genArgToolkitDef ", " fn.args) <> "\n"
        <> i3 <> "{ out : " <> fn.returns <> " }\n"
        <> i3 <> "$ Fn.make $ pure unit"


genArgToolkitDef :: Maybe QD.Argument -> String
genArgToolkitDef (Just arg) =
    arg.name <> " : " <> (fromMaybe "?" arg.default)
genArgToolkitDef Nothing =
    "?"


genArgTypeDef :: Maybe QD.Argument -> String
genArgTypeDef (Just arg) =
    arg.name <> " :: " <> (fromMaybe "Unknown" arg.type)
genArgTypeDef Nothing =
    "?"