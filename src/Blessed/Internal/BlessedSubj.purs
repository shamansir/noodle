module Blessed.Internal.BlessedSubj where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Symbol (reflectSymbol)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

data Subject
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



instance EncodeJson Subject where
    encodeJson = encodeJson <<< toString



fromString ∷ String → Maybe Subject
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


toString ∷ Subject → String
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



data SubjectKind
data ExtSubjectKind


foreign import data ScreenK :: SubjectKind
foreign import data ElementK :: SubjectKind
foreign import data BoxK :: SubjectKind
foreign import data TextK :: SubjectKind
foreign import data LineK :: SubjectKind
foreign import data BigTextK :: SubjectKind
foreign import data ListK :: SubjectKind
foreign import data FileManagerK :: SubjectKind
foreign import data ListTableK :: SubjectKind
foreign import data ListBarK :: SubjectKind
foreign import data FormK :: SubjectKind
foreign import data InputK :: SubjectKind
foreign import data TextAreaK :: SubjectKind
foreign import data TextBoxK :: SubjectKind
foreign import data ButtonK :: SubjectKind
foreign import data CheckboxK :: SubjectKind
foreign import data RadioSetK :: SubjectKind
foreign import data RadioButtonK :: SubjectKind
foreign import data PromptK :: SubjectKind
foreign import data QuestionK :: SubjectKind
foreign import data MessageK :: SubjectKind
foreign import data LoadingK :: SubjectKind
foreign import data ProgressBarK :: SubjectKind
foreign import data LogK :: SubjectKind
foreign import data TableK :: SubjectKind
foreign import data TerminalK :: SubjectKind
foreign import data ExtK :: ExtSubjectKind -> SubjectKind
foreign import data ProcessK :: ExtSubjectKind



screenK :: Proxy ScreenK
screenK = Proxy

elementK :: Proxy ElementK
elementK = Proxy

boxK :: Proxy BoxK
boxK = Proxy

textK :: Proxy TextK
textK = Proxy

lineK :: Proxy LineK
lineK = Proxy

bigtextK :: Proxy BigTextK
bigtextK = Proxy

listK :: Proxy ListK
listK = Proxy

filemanagerK :: Proxy FileManagerK
filemanagerK = Proxy

listtableK :: Proxy ListTableK
listtableK = Proxy

listbarK :: Proxy ListBarK
listbarK = Proxy

formK :: Proxy FormK
formK = Proxy

inputK :: Proxy InputK
inputK = Proxy

textareaK :: Proxy TextAreaK
textareaK = Proxy

textboxK :: Proxy TextBoxK
textboxK = Proxy

buttonK :: Proxy ButtonK
buttonK = Proxy

checkboxK :: Proxy CheckboxK
checkboxK = Proxy

radiosetK :: Proxy RadioSetK
radiosetK = Proxy

radiobuttonK :: Proxy RadioButtonK
radiobuttonK = Proxy

promptK :: Proxy PromptK
promptK = Proxy

questionK :: Proxy QuestionK
questionK = Proxy

messageK :: Proxy MessageK
messageK = Proxy

loadingK :: Proxy LoadingK
loadingK = Proxy

progressbarK :: Proxy ProgressBarK
progressbarK = Proxy

logK :: Proxy LogK
logK = Proxy

tableK :: Proxy TableK
tableK = Proxy

terminalK :: Proxy TerminalK
terminalK = Proxy

processK :: Proxy (ExtK ProcessK)
processK = Proxy


class IsSubject :: SubjectKind -> Constraint
class IsSubject a where
  reflectSubject :: Proxy a -> Subject


instance IsSubject ScreenK where reflectSubject _ = Screen
instance IsSubject ElementK where reflectSubject _ = Element
instance IsSubject BoxK where reflectSubject _ = Box
instance IsSubject TextK where reflectSubject _ = Text
instance IsSubject LineK where reflectSubject _ = Line
instance IsSubject BigTextK where reflectSubject _ = BigText
instance IsSubject ListK where reflectSubject _ = List
instance IsSubject FileManagerK where reflectSubject _ = FileManager
instance IsSubject ListTableK where reflectSubject _ = ListTable
instance IsSubject ListBarK where reflectSubject _ = ListBar
instance IsSubject FormK where reflectSubject _ = Form
instance IsSubject InputK where reflectSubject _ = Input
instance IsSubject TextAreaK where reflectSubject _ = TextArea
instance IsSubject TextBoxK where reflectSubject _ = TextBox
instance IsSubject ButtonK where reflectSubject _ = Button
instance IsSubject CheckboxK where reflectSubject _ = Checkbox
instance IsSubject RadioSetK where reflectSubject _ = RadioSet
instance IsSubject RadioButtonK where reflectSubject _ = RadioButton
instance IsSubject PromptK where reflectSubject _ = Prompt
instance IsSubject QuestionK where reflectSubject _ = Question
instance IsSubject MessageK where reflectSubject _ = Message
instance IsSubject LoadingK where reflectSubject _ = Loading
instance IsSubject ProgressBarK where reflectSubject _ = ProgressBar
instance IsSubject LogK where reflectSubject _ = Log
instance IsSubject TableK where reflectSubject _ = Table
instance IsSubject TerminalK where reflectSubject _ = Terminal




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