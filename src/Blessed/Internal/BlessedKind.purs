module Blessed.Internal.BlessedKind where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Symbol (reflectSymbol)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

data NKind
    = Node
    | Screen
    | Element
    | Box
    | Text
    | Line
    | BigText
    | List
    | FileManager
    | ListTable
    | ListBar
    | Form
    | Input
    | TextArea
    | TextBox
    | Button
    | Checkbox
    | RadioSet
    | RadioButton
    | Prompt
    | Question
    | Message
    | Loading
    | ProgressBar
    | Log
    | Table
    | Terminal
    | Ext String



instance EncodeJson NKind where
    encodeJson = encodeJson <<< toString



fromString ∷ String → Maybe NKind
fromString =
    case _ of
        "node" -> Just Node
        "screen" -> Just Screen
        "element" -> Just Element
        "box" -> Just Box
        "text" -> Just Text
        "line" -> Just Line
        "bigtext" -> Just BigText
        "list" -> Just List
        "filemanager" -> Just FileManager
        "listtable" -> Just ListTable
        "listbar" -> Just ListBar
        "form" -> Just Form
        "input" -> Just Input
        "textarea" -> Just TextArea
        "textbox" -> Just TextBox
        "button" -> Just Button
        "checkbox" -> Just Checkbox
        "radioset" -> Just RadioSet
        "radiobutton" -> Just RadioButton
        "prompt" -> Just Prompt
        "question" -> Just Question
        "message" -> Just Message
        "loading" -> Just Loading
        "progressbar" -> Just ProgressBar
        "log" -> Just Log
        "table" -> Just Table
        "terminal" -> Just Terminal
        k ->
            if (String.take 3 k == "ext")
                then  Just $ Ext $ String.drop 4 k
                else Nothing


toString ∷ NKind → String
toString =
    case _ of
        Node -> "node"
        Screen -> "screen"
        Element -> "element"
        Box -> "box"
        Text -> "text"
        Line -> "line"
        BigText -> "bigtext"
        List -> "list"
        FileManager -> "filemanager"
        ListTable -> "listtable"
        ListBar -> "listbar"
        Form -> "form"
        Input -> "input"
        TextArea -> "textarea"
        TextBox -> "textbox"
        Button -> "button"
        Checkbox -> "checkbox"
        RadioSet -> "radioset"
        RadioButton -> "radiobutton"
        Prompt -> "prompt"
        Question -> "question"
        Message -> "message"
        Loading -> "loading"
        ProgressBar -> "progressbar"
        Log -> "log"
        Table -> "table"
        Terminal -> "terminal"
        Ext str -> "ext:" <> str





-- An example using the Boolean-like data type YesNo:
data YesNo = Yes | No

data YesNoKind
foreign import data YesK :: YesNoKind
foreign import data NoK  :: YesNoKind

{-
Read yesK and noK as:
  yesK = (YesNoProxyValue :: YesNoProxy Yes) - a value of type "YesNoProxy Yes"
  noK  = (YesNoProxyValue :: YesNoProxy No)  - a value of type "YesNoProxy No" -}
yesK :: Proxy YesK
yesK = Proxy

noK :: Proxy NoK
noK = Proxy

class IsYesNoKind :: YesNoKind -> Constraint
class IsYesNoKind a where
  reflectYesNo :: Proxy a -> YesNo

instance IsYesNoKind YesK where
-- reflectYesNo (Proxy :: Proxy Yes) = Yes
   reflectYesNo _                    = Yes

instance IsYesNoKind NoK where
-- reflectYesNo (Proxy :: Proxy No) = No
   reflectYesNo _                   = No


-- We can also use instance chains here to distinguish
-- one from another

class IsYes :: YesNoKind -> Constraint
class IsYes a where
  isYes :: Proxy a -> YesNo

instance IsYes YesK where
  isYes _ = Yes
else instance IsYes a where
  isYes _ = No

-- Using instance chains here is more convenient if we had
-- a lot more type-level values than just 2. In some cases,
-- it is needed in cases where a type-level type can have an
-- infinite number of values, such as a type-level String

-- Open a REPL, import this module, and then run this code:
--    reflectYesNo yesK
--    reflectYesNo noK
--    isYes yesK
--    isYes noK


-- necessary for not getting errors while trying the functions in the REPL

instance Show YesNo where
    show Yes = "Yes"
    show No  = "No"