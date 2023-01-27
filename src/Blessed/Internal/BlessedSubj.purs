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
    | Image
    | AnsiImage
    | OverlayImage
    | Video
    | Layout
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
        "image" -> Just Image
        "ansiimage" -> Just AnsiImage
        "overlayimage" -> Just OverlayImage
        "video" -> Just Video
        "layout" -> Just Layout
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
        Image -> "image"
        AnsiImage -> "ansiimage"
        OverlayImage -> "overlayimage"
        Video -> "video"
        Layout -> "layout"
        Ext str -> "ext:" <> str



data SubjectKind
data ExtSubjectKind


foreign import data NodeK :: SubjectKind
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
foreign import data ImageK :: SubjectKind
foreign import data AnsiImageK :: SubjectKind
foreign import data OverlayImageK :: SubjectKind
foreign import data VideoK :: SubjectKind
foreign import data LayoutK :: SubjectKind
foreign import data ExtK :: ExtSubjectKind -> SubjectKind
foreign import data ProcessK :: ExtSubjectKind



nodeK :: Proxy NodeK
nodeK = Proxy

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

imageK :: Proxy ImageK
imageK = Proxy

ansiimageK :: Proxy AnsiImageK
ansiimageK = Proxy

overlayimageK :: Proxy OverlayImageK
overlayimageK = Proxy

videoK :: Proxy VideoK
videoK = Proxy

layoutK :: Proxy LayoutK
layoutK = Proxy

processK :: Proxy (ExtK ProcessK)
processK = Proxy


class IsSubject :: SubjectKind -> Constraint
class IsSubject a where
  reflectSubject :: Proxy a -> Subject


instance IsSubject NodeK where reflectSubject _ = Node
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


class Fires :: forall event'. SubjectKind -> event' -> Constraint
class Fires (subj :: SubjectKind) event


class Extends (parent :: SubjectKind) (child :: SubjectKind)


instance Extends NodeK ScreenK
instance Extends NodeK ElementK

instance Extends NodeK BoxK
instance Extends ElementK BoxK

instance Extends NodeK TextK
instance Extends ElementK TextK

instance Extends NodeK LayoutK
instance Extends ElementK LayoutK

instance Extends NodeK LineK
instance Extends ElementK LineK
instance Extends BoxK LineK

instance Extends NodeK BigTextK
instance Extends ElementK BigTextK
instance Extends BoxK BigTextK

instance Extends NodeK ListK
instance Extends ElementK ListK
instance Extends BoxK ListK

instance Extends NodeK ListBarK
instance Extends ElementK ListBarK
instance Extends BoxK ListBarK

instance Extends NodeK FormK
instance Extends ElementK FormK
instance Extends BoxK FormK

instance Extends NodeK InputK
instance Extends ElementK InputK
instance Extends BoxK InputK

instance Extends NodeK RadioSetK
instance Extends ElementK RadioSetK
instance Extends BoxK RadioSetK

instance Extends NodeK PromptK
instance Extends ElementK PromptK
instance Extends BoxK PromptK

instance Extends NodeK QuestionK
instance Extends ElementK QuestionK
instance Extends BoxK QuestionK

instance Extends NodeK MessageK
instance Extends ElementK MessageK
instance Extends BoxK MessageK

instance Extends NodeK LoadingK
instance Extends ElementK LoadingK
instance Extends BoxK LoadingK

instance Extends NodeK TableK
instance Extends ElementK TableK
instance Extends BoxK TableK

instance Extends NodeK TerminalK
instance Extends ElementK TerminalK
instance Extends BoxK TerminalK

instance Extends NodeK ImageK
instance Extends ElementK ImageK
instance Extends BoxK ImageK

instance Extends NodeK AnsiImageK
instance Extends ElementK AnsiImageK
instance Extends BoxK AnsiImageK

instance Extends NodeK OverlayImageK
instance Extends ElementK OverlayImageK
instance Extends BoxK OverlayImageK

instance Extends NodeK VideoK
instance Extends ElementK VideoK
instance Extends BoxK VideoK

instance Extends NodeK FileManagerK
instance Extends ElementK FileManagerK
instance Extends BoxK FileManagerK
instance Extends ListK FileManagerK

instance Extends NodeK ListTableK
instance Extends ElementK ListTableK
instance Extends BoxK ListTableK
instance Extends ListK ListTableK

instance Extends NodeK TextAreaK
instance Extends ElementK TextAreaK
instance Extends BoxK TextAreaK
instance Extends InputK TextAreaK

instance Extends NodeK ButtonK
instance Extends ElementK ButtonK
instance Extends BoxK ButtonK
instance Extends InputK ButtonK

instance Extends NodeK CheckboxK
instance Extends ElementK CheckboxK
instance Extends BoxK CheckboxK
instance Extends InputK CheckboxK

instance Extends NodeK ProgressBarK
instance Extends ElementK ProgressBarK
instance Extends BoxK ProgressBarK
instance Extends InputK ProgressBarK

instance Extends NodeK TextBoxK
instance Extends ElementK TextBoxK
instance Extends BoxK TextBoxK
instance Extends InputK TextBoxK
instance Extends TextAreaK TextBoxK

instance Extends NodeK RadioButtonK
instance Extends ElementK RadioButtonK
instance Extends BoxK RadioButtonK
instance Extends InputK RadioButtonK
instance Extends CheckboxK RadioButtonK