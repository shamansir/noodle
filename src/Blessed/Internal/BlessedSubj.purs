module Blessed.Internal.BlessedSubj where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Symbol (reflectSymbol)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

data Subject_
    = Node_
    | Screen_
    | Element_
    | Box_
    | Text_
    | Line_
    | BigText_
    | List_
    | FileManager_
    | ListTable_
    | ListBar_
    | Form_
    | Input_
    | TextArea_
    | TextBox_
    | Button_
    | Checkbox_
    | RadioSet_
    | RadioButton_
    | Prompt_
    | Question_
    | Message_
    | Loading_
    | ProgressBar_
    | Log_
    | Table_
    | Terminal_
    | Image_
    | AnsiImage_
    | OverlayImage_
    | Video_
    | Layout_
    | Ext_ String



instance EncodeJson Subject_ where
    encodeJson = encodeJson <<< toString



fromString ∷ String → Maybe Subject_
fromString =
    case _ of
        "node" -> Just Node_
        "screen" -> Just Screen_
        "element" -> Just Element_
        "box" -> Just Box_
        "text" -> Just Text_
        "line" -> Just Line_
        "bigtext" -> Just BigText_
        "list" -> Just List_
        "filemanager" -> Just FileManager_
        "listtable" -> Just ListTable_
        "listbar" -> Just ListBar_
        "form" -> Just Form_
        "input" -> Just Input_
        "textarea" -> Just TextArea_
        "textbox" -> Just TextBox_
        "button" -> Just Button_
        "checkbox" -> Just Checkbox_
        "radioset" -> Just RadioSet_
        "radiobutton" -> Just RadioButton_
        "prompt" -> Just Prompt_
        "question" -> Just Question_
        "message" -> Just Message_
        "loading" -> Just Loading_
        "progressbar" -> Just ProgressBar_
        "log" -> Just Log_
        "table" -> Just Table_
        "terminal" -> Just Terminal_
        "image" -> Just Image_
        "ansiimage" -> Just AnsiImage_
        "overlayimage" -> Just OverlayImage_
        "video" -> Just Video_
        "layout" -> Just Layout_
        k ->
            if (String.take 3 k == "ext")
                then  Just $ Ext_ $ String.drop 4 k
                else Nothing


toString ∷ Subject_ → String
toString =
    case _ of
        Node_ -> "node"
        Screen_ -> "screen"
        Element_ -> "element"
        Box_ -> "box"
        Text_ -> "text"
        Line_ -> "line"
        BigText_ -> "bigtext"
        List_ -> "list"
        FileManager_ -> "filemanager"
        ListTable_ -> "listtable"
        ListBar_ -> "listbar"
        Form_ -> "form"
        Input_ -> "input"
        TextArea_ -> "textarea"
        TextBox_ -> "textbox"
        Button_ -> "button"
        Checkbox_ -> "checkbox"
        RadioSet_ -> "radioset"
        RadioButton_ -> "radiobutton"
        Prompt_ -> "prompt"
        Question_ -> "question"
        Message_ -> "message"
        Loading_ -> "loading"
        ProgressBar_ -> "progressbar"
        Log_ -> "log"
        Table_ -> "table"
        Terminal_ -> "terminal"
        Image_ -> "image"
        AnsiImage_ -> "ansiimage"
        OverlayImage_ -> "overlayimage"
        Video_ -> "video"
        Layout_ -> "layout"
        Ext_ str -> "ext:" <> str



data Subject
data ExtSubject


-- foreign import data Node :: Node

foreign import data Node :: Subject
foreign import data Screen :: Subject
foreign import data Element :: Subject
foreign import data Box :: Subject
foreign import data Text :: Subject
foreign import data Line :: Subject
foreign import data BigText :: Subject
foreign import data List :: Subject
foreign import data FileManager :: Subject
foreign import data ListTable :: Subject
foreign import data ListBar :: Subject
foreign import data Form :: Subject
foreign import data Input :: Subject
foreign import data TextArea :: Subject
foreign import data TextBox :: Subject
foreign import data Button :: Subject
foreign import data Checkbox :: Subject
foreign import data RadioSet :: Subject
foreign import data RadioButton :: Subject
foreign import data Prompt :: Subject
foreign import data Question :: Subject
foreign import data Message :: Subject
foreign import data Loading :: Subject
foreign import data ProgressBar :: Subject
foreign import data Log :: Subject
foreign import data Table :: Subject
foreign import data Terminal :: Subject
foreign import data Image :: Subject
foreign import data AnsiImage :: Subject
foreign import data OverlayImage :: Subject
foreign import data Video :: Subject
foreign import data Layout :: Subject
foreign import data Ext :: ExtSubject -> Subject
foreign import data Process :: ExtSubject


node :: Proxy Node
node = Proxy

screen :: Proxy Screen
screen = Proxy

element :: Proxy Element
element = Proxy

box :: Proxy Box
box = Proxy

text :: Proxy Text
text = Proxy

line :: Proxy Line
line = Proxy

bigtext :: Proxy BigText
bigtext = Proxy

list :: Proxy List
list = Proxy

filemanager :: Proxy FileManager
filemanager = Proxy

listtable :: Proxy ListTable
listtable = Proxy

listbar :: Proxy ListBar
listbar = Proxy

form :: Proxy Form
form = Proxy

input :: Proxy Input
input = Proxy

textarea :: Proxy TextArea
textarea = Proxy

textbox :: Proxy TextBox
textbox = Proxy

button :: Proxy Button
button = Proxy

checkbox :: Proxy Checkbox
checkbox = Proxy

radioset :: Proxy RadioSet
radioset = Proxy

radiobutton :: Proxy RadioButton
radiobutton = Proxy

prompt :: Proxy Prompt
prompt = Proxy

question :: Proxy Question
question = Proxy

message :: Proxy Message
message = Proxy

loading :: Proxy Loading
loading = Proxy

progressbar :: Proxy ProgressBar
progressbar = Proxy

log :: Proxy Log
log = Proxy

table :: Proxy Table
table = Proxy

terminal :: Proxy Terminal
terminal = Proxy

image :: Proxy Image
image = Proxy

ansiimage :: Proxy AnsiImage
ansiimage = Proxy

overlayimage :: Proxy OverlayImage
overlayimage = Proxy

video :: Proxy Video
video = Proxy

layout :: Proxy Layout
layout = Proxy

process :: Proxy (Ext Process)
process = Proxy


class IsSubject :: Subject -> Constraint
class IsSubject a where
  reflectSubject :: Proxy a -> Subject_

{-
class IsSubject :: Subject -> Constraint
class IsSubject a where
  reflectSubject :: Proxy a -> Subject_
  reify :: Subject_ -> Maybe (Proxy a)


instance IsSubject Node where
    reflectSubject _ = Node_
    reify Node_ = Just node
    reify _ = Nothing

instance IsSubject Screen where
    reflectSubject _ = Screen_
    reify Screen_ = Just screen
    reify _ = Nothing
-}

instance IsSubject Node where reflectSubject _ = Node_
instance IsSubject Screen where reflectSubject _ = Screen_
instance IsSubject Element where reflectSubject _ = Element_
instance IsSubject Box where reflectSubject _ = Box_
instance IsSubject Text where reflectSubject _ = Text_
instance IsSubject Line where reflectSubject _ = Line_
instance IsSubject BigText where reflectSubject _ = BigText_
instance IsSubject List where reflectSubject _ = List_
instance IsSubject FileManager where reflectSubject _ = FileManager_
instance IsSubject ListTable where reflectSubject _ = ListTable_
instance IsSubject ListBar where reflectSubject _ = ListBar_
instance IsSubject Form where reflectSubject _ = Form_
instance IsSubject Input where reflectSubject _ = Input_
instance IsSubject TextArea where reflectSubject _ = TextArea_
instance IsSubject TextBox where reflectSubject _ = TextBox_
instance IsSubject Button where reflectSubject _ = Button_
instance IsSubject Checkbox where reflectSubject _ = Checkbox_
instance IsSubject RadioSet where reflectSubject _ = RadioSet_
instance IsSubject RadioButton where reflectSubject _ = RadioButton_
instance IsSubject Prompt where reflectSubject _ = Prompt_
instance IsSubject Question where reflectSubject _ = Question_
instance IsSubject Message where reflectSubject _ = Message_
instance IsSubject Loading where reflectSubject _ = Loading_
instance IsSubject ProgressBar where reflectSubject _ = ProgressBar_
instance IsSubject Log where reflectSubject _ = Log_
instance IsSubject Table where reflectSubject _ = Table_
instance IsSubject Terminal where reflectSubject _ = Terminal_
instance IsSubject (Ext Process) where reflectSubject _ = Ext_ "process"


class Fires :: forall event'. Subject -> event' -> Constraint
class Fires (subj :: Subject) event



class Extends (parent :: Subject) (child :: Subject)


instance Extends Node Screen
instance Extends Node Element
instance Extends Node Node
instance Extends Screen Screen

instance Extends Node Box
instance Extends Element Box
instance Extends Box Box

instance Extends Node Text
instance Extends Element Text
instance Extends Text Text

instance Extends Node Layout
instance Extends Element Layout
instance Extends Layout Layout

instance Extends Node Line
instance Extends Element Line
instance Extends Box Line
instance Extends Line Line

instance Extends Node BigText
instance Extends Element BigText
instance Extends Box BigText
instance Extends BigText BigText

instance Extends Node List
instance Extends Element List
instance Extends Box List
instance Extends List List

instance Extends Node ListBar
instance Extends Element ListBar
instance Extends Box ListBar
instance Extends List ListBar -- FIXME: not documented
instance Extends ListBar ListBar

instance Extends Node Form
instance Extends Element Form
instance Extends Box Form
instance Extends Form Form

instance Extends Node Input
instance Extends Element Input
instance Extends Box Input
instance Extends Input Input

instance Extends Node RadioSet
instance Extends Element RadioSet
instance Extends Box RadioSet
instance Extends RadioSet RadioSet

instance Extends Node Prompt
instance Extends Element Prompt
instance Extends Box Prompt
instance Extends Prompt Prompt

instance Extends Node Question
instance Extends Element Question
instance Extends Box Question
instance Extends Question Question

instance Extends Node Message
instance Extends Element Message
instance Extends Box Message
instance Extends Message Message

instance Extends Node Loading
instance Extends Element Loading
instance Extends Box Loading
instance Extends Loading Loading

instance Extends Node Table
instance Extends Element Table
instance Extends Box Table
instance Extends Table Table

instance Extends Node Terminal
instance Extends Element Terminal
instance Extends Box Terminal
instance Extends Terminal Terminal

instance Extends Node Image
instance Extends Element Image
instance Extends Box Image
instance Extends Image Image

instance Extends Node AnsiImage
instance Extends Element AnsiImage
instance Extends Box AnsiImage
instance Extends AnsiImage AnsiImage

instance Extends Node OverlayImage
instance Extends Element OverlayImage
instance Extends Box OverlayImage
instance Extends OverlayImage OverlayImage

instance Extends Node Video
instance Extends Element Video
instance Extends Box Video
instance Extends Video Video

instance Extends Node FileManager
instance Extends Element FileManager
instance Extends Box FileManager
instance Extends List FileManager
instance Extends FileManager FileManager

instance Extends Node ListTable
instance Extends Element ListTable
instance Extends Box ListTable
instance Extends List ListTable
instance Extends ListTable ListTable

instance Extends Node TextArea
instance Extends Element TextArea
instance Extends Box TextArea
instance Extends Input TextArea
instance Extends TextArea TextArea

instance Extends Node Button
instance Extends Element Button
instance Extends Box Button
instance Extends Input Button
instance Extends Button Button

instance Extends Node Checkbox
instance Extends Element Checkbox
instance Extends Box Checkbox
instance Extends Input Checkbox
instance Extends Checkbox Checkbox

instance Extends Node ProgressBar
instance Extends Element ProgressBar
instance Extends Box ProgressBar
instance Extends Input ProgressBar
instance Extends ProgressBar ProgressBar

instance Extends Node TextBox
instance Extends Element TextBox
instance Extends Box TextBox
instance Extends Input TextBox
instance Extends TextArea TextBox
instance Extends TextBox ProgressBar

instance Extends Node RadioButton
instance Extends Element RadioButton
instance Extends Box RadioButton
instance Extends Input RadioButton
instance Extends Checkbox RadioButton
instance Extends RadioButton RadioButton