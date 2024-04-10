module Test.Formatting where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Text.Output.Blessed (render) as T
import Data.Text.Format as F
import Data.Text.Doc (Doc(..))
import Data.Text.Doc as D
import Data.Text.Doc2 as D2
import Data.String as String
import Data.String.Extra (words) as String
import Data.Array (concat, take) as Array
import Data.List (List, (:))
import Data.List (List(..)) as List

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


{-
docIndentedSamples :: Array (Doc /\ String)
docIndentedSamples =
    [ (D.s "foo" <> D.br) /\ "foo\n"
    , (D.nest 1 $ D.s "foo" ) /\ "    foo"
    , (D.nest 1 $ D.para [ D.s "foo", D.s "bar", D.s "buz" ] ) /\ "    foo\n    bar\n    buz"
    , (D.nest 1 $ D.pair (D.l "test") $ D.nest 2 $ D.para [ D.s "foo", D.s "bar", D.s "buz" ] )
        /\ """    test
            foo
            bar
            buz"""
    , (D.nest 1 $ D.pair (D.para [ D.s "test" ] <> D.br) $ D.nest 2 $ D.para [ D.s "foo", D.s "bar", D.s "buz" ] )
        /\ """    test
            foo
            bar
            buz"""
    , (D.mark "*" $ D.s "foo") /\ "* foo"
    , (D.ws "'" "`" $ D.s "foo") /\ "'foo`"
    , (D.ww "\"" $ D.s "foo") /\ "\"foo\""
    , (D.w "[" "]" $ D.s "foo" <> (D.mark "," $ D.s "bar") <> (D.mark "," $ D.s "buz")) /\
        "[ foo, bar, buz ]"
    , (D.s "func" <> (D.ws "(" ")" $ D.s "foo" <> (D.mark "," $ D.s "bar") <> (D.mark "," $ D.s "buz"))) /\
        "func(foo, bar, buz)"
    , (D.nest 1 $ D.wrap (D.s "[" <> D.sp) (D.s "]") $ D.para [ D.s "foo", D.mark "," $ D.s "bar", D.mark "," $ D.s "buz" <> D.br ])
        /\ """    [ foo
    , bar
    , buz
    ]"""
    , (D.nest 1 $ D.para [ D.s "subj", D.mark "|>" $ D.s "modify 1", D.mark "|>" $ D.s "foo" <> D.br ])
        /\ """    subj
    |> modify 1
    |> foo
"""
    , (D.nest 0 (D.s "subj") <> D.br <> (D.nest 1 $ D.para [ D.mark "|>" $ D.s "modify 1", D.mark "|>" $ D.s "foo" <> D.br ]))
        /\ """subj
    |> modify 1
    |> foo
"""
    , (D.para [ D.mark "-" $ D.s "Item 1", D.mark "-" $ D.s "Item 2", D.mark "-" $ D.s "Item 3" ]) /\
        """- Item 1
- Item 2
- Item 3"""
    , (D.nest 0 $ D.para
        [ D.mark "*" $ D.s "Item 1"
        , (D.mark "*" $ D.s "Item 2")
            <> D.br
            <> D.nest 1
                (D.para
                    [ D.mark "-" $ D.s "Item 2.1"
                    , D.mark "-" $ D.s "Item 2.2"
                    , D.mark "-" $ D.s "Item 2.3"
                        <> D.br
                        <> D.nest 1 (D.mark "o" $ D.s "Item 2.3.1")
                    , D.mark "-" $ D.s "Item 2.4"
                    ])
        , D.mark "*" $ D.s "Item 3"
        ]) /\ """* Item 1
* Item 2
   - Item 2.1
   - Item 2.2
   - Item 2.3
       o Item 2.3.1
* Item 3"""
    ] -}


docIndentedSamples :: Array (D2.DOC /\ String)
docIndentedSamples =
    [ (D2.text "foo" <> D2.line) /\ "foo\n"
    , (D2.nest 1 $ D2.text "foo" ) /\ "    foo"
    , (D2.nest 1 $ D2.stack [ D2.text "foo", D2.text "bar", D2.text "buz" ] ) /\ "    foo\n    bar\n    buz"
    , (D2.nest 1 $ D2.concat (D2.text "test" <> D2.line) $ D2.nest 2 $ D2.stack [ D2.text "foo", D2.text "bar", D2.text "buz" ] )
        /\ """    test
            foo
            bar
            buz"""
    , (D2.nest 1 $ D2.concat (D2.stack [ D2.text "test" ] <> D2.line) $ D2.nest 2 $ D2.stack [ D2.text "foo", D2.text "bar", D2.text "buz" ] )
        /\ """    test
            foo
            bar
            buz"""
    , (D2.mark "*" $ D2.text "foo") /\ "* foo"
    , (D2.wbracket "'" "`" $ D2.text "foo") /\ "'foo`"
    , (D2.wrap "\"" $ D2.text "foo") /\ "\"foo\""
    , (D2.wbracket "[" "]" $ D2.text "foo" <> (D2.mark "," $ D2.text "bar") <> (D2.mark "," $ D2.text "buz")) /\
        "[ foo, bar, buz ]"
    , (D2.text "func" <> (D2.wbracket "(" ")" $ D2.text "foo" <> (D2.mark "," $ D2.text "bar") <> (D2.mark "," $ D2.text "buz"))) /\
        "func(foo, bar, buz)"
    , (D2.nest 1 $ D2.wbracket "[ " "]" $ D2.stack [ D2.text "foo", D2.mark "," $ D2.text "bar", D2.mark "," $ D2.text "buz" <> D2.line ])
        /\ """    [ foo
    , bar
    , buz
    ]"""
    , (D2.nest 1 $ D2.stack [ D2.text "subj", D2.mark "|>" $ D2.text "modify 1", D2.mark "|>" $ D2.text "foo" <> D2.line ])
        /\ """    subj
    |> modify 1
    |> foo
"""
    , (D2.nest 0 (D2.text "subj") <> D2.line <> (D2.nest 1 $ D2.stack [ D2.mark "|>" $ D2.text "modify 1", D2.mark "|>" $ D2.text "foo" <> D2.line ]))
        /\ """subj
    |> modify 1
    |> foo
"""
    , (D2.stack [ D2.mark "-" $ D2.text "Item 1", D2.mark "-" $ D2.text "Item 2", D2.mark "-" $ D2.text "Item 3" ]) /\
        """- Item 1
- Item 2
- Item 3"""
    , (D2.nest 0 $ D2.stack
        [ D2.mark "*" $ D2.text "Item 1"
        , (D2.mark "*" $ D2.text "Item 2")
            <> D2.line
            <> D2.nest 1
                (D2.stack
                    [ D2.mark "-" $ D2.text "Item 2.1"
                    , D2.mark "-" $ D2.text "Item 2.2"
                    , D2.mark "-" $ D2.text "Item 2.3"
                        <> D2.line
                        <> D2.nest 2 (D2.mark "o" $ D2.text "Item 2.3.1")
                    , D2.mark "-" $ D2.text "Item 2.4"
                    ])
        , D2.mark "*" $ D2.text "Item 3"
        ]) /\ """* Item 1
* Item 2
   - Item 2.1
   - Item 2.2
   - Item 2.3
       o Item 2.3.1
* Item 3"""
    , showTree tree /\ ""
    , showTree' tree /\ ""
    , showXML xml /\ ""
    ]


blessedSamples :: Array (F.Tag /\ String)
blessedSamples =
    [ F.s "foo" /\ "foo"
    , F.bolds "test" /\ "{bold}test{/bold}"
    ]


spec :: Spec Unit
spec = do

  describe "Formatting works properly for Doc" $ do

    -- helper { title : \idx (doc /\ _) -> show idx <> " : " <> show doc, render : D.layout 0 } docIndentedSamples
    helper { title : \idx (doc /\ _) -> show idx <> " : " <> String.take 200 (show doc), render : D2.pretty 30 } docIndentedSamples

  describe "Formatting works properly for Blessed" $ do

    helper { title : \idx (_ /\ expected) -> show idx <> " : " <> expected, render : T.render } blessedSamples



helper :: forall a. { title :: Int -> a /\ String -> String, render :: a -> String } -> Array (a /\ String) -> Spec Unit
helper { title, render } =
    foldlWithIndex
        (\idx prev (tag /\ expected) -> do
            prev
            *>
            (it ("works for sample " <> title idx (tag /\ expected)) $
                render tag `shouldEqual` expected
            )
        )
        (pure unit)


data Tree = Node String (List Tree)


showTree :: Tree -> D2.DOC
showTree (Node s ts) = D2.group $ D2.text s <> D2.nest (String.length s) (showBracket ts)


showBracket :: List Tree -> D2.DOC
showBracket List.Nil = D2.nil
showBracket ts = D2.text "[" <> D2.nest 1 (showTrees ts) <> D2.text "]"


showTrees :: List Tree -> D2.DOC
showTrees List.Nil = D2.nil
showTrees (List.Cons t List.Nil) = showTree t
showTrees (List.Cons t ts) = showTree t <> D2.text "," <> D2.line <> showTrees ts


showTree' :: Tree -> D2.DOC
showTree' (Node s ts) = D2.text s <> showBracket' ts


showBracket' :: List Tree -> D2.DOC
showBracket' List.Nil = D2.nil
showBracket' ts = D2.bracket "[" (showTrees' ts) "]"


showTrees' :: List Tree -> D2.DOC
showTrees' List.Nil = D2.nil
showTrees' (List.Cons t List.Nil) = showTree' t
showTrees' (List.Cons t ts) = showTree' t <> D2.text "," <> D2.line <> showTrees' ts


tree :: Tree
tree =
    Node "aaa"
        ( Node "bbb"
            ( Node "ccc" List.Nil
            : Node "dd" List.Nil
            : List.Nil
            )
        : Node "eee" List.Nil
        : Node "fff"
            ( Node "gg" List.Nil
            : Node "hhh" List.Nil
            : Node "ii" List.Nil
            : List.Nil
            )
        : List.Nil
        )


testtree :: Int -> String
testtree w = D2.pretty w $ showTree tree
testtree' :: Int -> String
testtree' w = D2.pretty w $ showTree' tree


data XML
    = Elt String (Array Att) (Array XML)
    | Txt String


data Att = Att String String


showXML :: XML -> D2.DOC
showXML x = D2.folddoc (<>) (showXMLs x)


showXMLs :: XML -> Array D2.DOC
showXMLs (Elt n a []) = [ D2.text "<" <> showTag n a <> D2.text "/>" ]
showXMLs (Elt n a c)  =
    [ D2.text "<" <> showTag n a <> D2.text ">" <>
        showFill showXMLs c <>
      D2.text "</" <> D2.text n <> D2.text "/>"
    ]
showXMLs (Txt s) = map D2.text (String.words s)


showAtts :: Att -> Array D2.DOC
showAtts (Att n v) = [ D2.text n <> D2.text "=" <> D2.text (quoted v) ]


quoted :: String -> String
quoted s = "\"" <> s <> "\""

showTag :: String -> Array Att -> D2.DOC
showTag n a = D2.text n <> showFill showAtts a

showFill :: forall a. (a -> Array D2.DOC) -> Array a -> D2.DOC
showFill _ [] = D2.nil
showFill f xs = D2.bracket "" (D2.fill $ Array.concat $ map f xs) ""


xml :: XML
xml =
    Elt "p" [
        Att "color" "red",
        Att "font" "Times",
        Att "size" "10"
    ] [
        Txt "Here is some",
        Elt "em" [] [
            Txt "emphasized"
        ],
        Txt "text.",
        Txt "Here is a",
        Elt "a" [
            Att "href" "http://www.eg.com"
        ] [
            Txt "link"
        ],
        Txt "elsewhere."
    ]


testXML :: Int -> String
testXML w = D2.pretty w $ showXML xml