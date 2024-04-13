module Data.Text.Doc where

import Prelude

import Data.Array ((:))
import Data.Array (mapWithIndex, intersperse, replicate, uncons) as Array
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar, singleton, fromCodePointArray, joinWith) as String


data Doc
    = Nil
    | Text String
    | Break
    | Indent
    | Para (Array Doc) -- TODO: merge into `Nest (Maybe Int) (Array Doc)`
    | Nest Int Doc
    | Pair Doc Doc


infixr 6 Nest as :<>


-- TODO: Indent size as an option
-- TODO: Word wrap
-- TODO: `Doc a` with `Value a` constructor


data Break
    = None
    | Space
    -- | WordWrap Int
    | All


data Indent
    = Empty
    | Spaces Int
    | Custom String
    | Tab


renderIndent :: Indent -> String
renderIndent Empty = ""
renderIndent (Spaces n) = String.fromCodePointArray $ Array.replicate n $ String.codePointFromChar ' '
renderIndent (Custom s) = s
renderIndent Tab = String.singleton $ String.codePointFromChar '\t'


renderBreak :: Break -> String
renderBreak None = ""
renderBreak Space = " "
renderBreak All = "\n"


type Options =
    { break :: Break
    , indent :: Indent
    }


type Options' =
    { break :: String
    , indent :: String
    }


render' :: Options' -> Int -> Doc -> String
render' opts level = case _ of
    Nil -> ""
    Text s -> s
    Break -> opts.break
    Indent -> opts.indent
    Para docs -> String.joinWith "" $ render' opts 0 <$> Array.intersperse (brIndent level) docs
    Nest n (Para docs) -> String.joinWith "" $ render' opts 0 <$> Array.mapWithIndex (adapt n) docs
    -- Nest _ (Nest m doc) -> render $ Pair (Pair Break $ mkIndent m) doc
    Nest n doc -> render' opts 0 $ adapt n 0 doc
    Pair a b -> render' opts 0 a <> render' opts 0 b
    where
        -- raw :: Doc -> String
        -- raw = render' opts 0 -- fails to run after compilation
        -- adapt n (Nest m x) = Nest (max 0 $ m - n + 1) $ adapt (max 0 $ m - n) x
        adapt _ 0 (Nest m x) = Nest m x
        adapt _ _ (Nest m x) = Pair Break $ Nest m x
        adapt n _ Break = brIndent n
        adapt n 0 doc = Pair (mkIndent n) doc
        adapt n _ doc = Pair (brIndent n) doc
        brIndent = Pair Break <<< mkIndent
        mkIndent 0 = Nil
        mkIndent n = Pair Indent $ mkIndent $ n - 1


makeOpts :: Options -> Options'
makeOpts { break, indent } =
    { break : renderBreak break
    , indent : renderIndent indent
    }


-- render :: Doc -> String
render :: Options -> Doc -> String
render opts = render' (makeOpts opts) 0


instance Semigroup Doc where
    append = Pair


text :: String -> Doc
text = Text
nil :: Doc
nil = Nil
break :: Doc
break = Break
nest :: Int -> Doc -> Doc
nest = Nest
nest' :: Int -> Array Doc -> Doc
nest' i = Nest i <<< Para
stack :: Array Doc -> Doc
stack = Para
concat :: Doc -> Doc -> Doc
concat = Pair
indent :: Doc
indent = Indent


space :: Doc
space = Text " "
mark :: String -> Doc -> Doc
mark s doc = Text s <> space <> doc
bracket :: String -> Doc -> String -> Doc
bracket l x r = Text l <> x <> Text r
wbracket :: String -> String -> Doc -> Doc
wbracket l = flip $ bracket l
wrap :: String -> Doc -> Doc
wrap q = wbracket q q


sp :: Doc -> Doc -> Doc
sp x y = x <> text " " <> y
br :: Doc -> Doc -> Doc
br x y = x <> break <> y
-- spbr :: Doc -> Doc -> Doc
-- spbr x y = x <> (break) <> y


infixr 6 sp as <+>
infixr 6 br as </>
-- infixr 6 spbr as <+/>


folddoc :: (Doc -> Doc -> Doc) -> Array Doc -> Doc
folddoc f arr =
    case Array.uncons arr of
        Nothing -> nil
        Just { head, tail } ->
            case tail of
                [] -> head
                _ -> f head $ folddoc f tail


instance Show Doc where
    show :: Doc -> String
    show = case _ of
        Nil -> "<nil>"
        -- Space -> "<sp>"
        Break -> "<br>"
        Indent -> "<ind>"
        Text str ->
            "<text(" <> str <> ")>"
        Para docs ->
            "<para(" <> (String.joinWith "," $ show <$> docs) <> ")>"
        Pair docA docB ->
            "<concat(" <> show docA <> "," <> show docB <> ")>"
        -- Marker str doc ->
        --     "<marker(" <> show str <> "," <> show doc <> ")>"
        -- Join sep docs ->
        --     "<join(" <> show sep <> ",(" <> (String.joinWith "," $ show <$> docs) <> "))>"
        -- Wrap { start, end } doc ->
        --     "<wrap(" <> show start <> "," <> show end <> "," <> show doc <> ")>"
        Nest nextIndent doc ->
            "<nest(" <> show nextIndent <> "," <> show doc <> ")>"