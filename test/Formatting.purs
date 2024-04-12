module Test.Formatting where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Text.Output.Blessed (render) as T
import Data.Text.Format as F
import Data.Text.Doc (Doc(..))
-- import Data.Text.Doc as D
import Data.Text.Doc3 as D
import Data.Text.Doc3 ((<+>), (</>))
import Data.String as String
import Data.String.Extra (words) as String
import Data.Array (concat, take, fromFoldable) as Array
import Data.List (List, (:))
import Data.List (List(..)) as List

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


docIndentedSamples :: Array (D.Doc /\ String)
docIndentedSamples =
    [ (D.text "foo" <> D.break) /\ "foo\n"
    , (D.nest 1 $ D.text "foo" ) /\ "<-->foo"
    , (D.break <> D.indent <> D.text "foo" ) /\ "\n<-->foo"
    , (D.nest 1 $ D.stack [ D.text "foo", D.text "bar", D.text "buz" ] ) /\ "<-->foo\n<-->bar\n<-->buz"
    , (D.nest 1 $ D.concat (D.text "test" <> D.break) $ D.nest 2 $ D.stack [ D.text "foo", D.text "bar", D.text "buz" ] )
        /\ """<-->test
<--><-->foo
<--><-->bar
<--><-->buz"""
    , (D.nest 1 $ D.concat (D.stack [ D.text "test" ] <> D.break) $ D.nest 2 $ D.stack [ D.text "foo", D.text "bar", D.text "buz" ] )
        /\ """<-->test
<--><-->foo
<--><-->bar
<--><-->buz"""
    , (D.mark "*" $ D.text "foo") /\ "* foo"
    , (D.wbracket "'" "`" $ D.text "foo") /\ "'foo`"
    , (D.wrap "\"" $ D.text "foo") /\ "\"foo\""
    , (D.wbracket "[" "]" $ D.text "foo" <> (D.mark "," $ D.text "bar") <> (D.mark "," $ D.text "buz")) /\
        "[foo, bar, buz]"
    , (D.text "func" <> (D.wbracket "(" ")" $ D.text "foo" <> (D.mark "," $ D.text "bar") <> (D.mark "," $ D.text "buz"))) /\
        "func(foo, bar, buz)"
    , (D.nest 1 $ D.stack [ D.mark "[" $ D.text "foo", D.mark "," $ D.text "bar", D.mark "," $ D.text "buz", D.text "]" ])
        /\ """<-->[ foo
<-->, bar
<-->, buz
<-->]"""
    , (D.nest 1 $ D.stack [ D.text "subj", D.mark "|>" $ D.text "modify 1", D.mark "|>" $ D.text "foo" <> D.break ])
        /\ """<-->subj
<-->|> modify 1
<-->|> foo
"""
    , (D.nest 0 (D.text "subj") <> D.break <> (D.nest 1 $ D.stack [ D.mark "|>" $ D.text "modify 1", D.mark "|>" $ D.text "foo" <> D.break ]))
        /\ """subj
<-->|> modify 1
<-->|> foo
"""
    , (D.stack [ D.mark "-" $ D.text "Item 1", D.mark "-" $ D.text "Item 2", D.mark "-" $ D.text "Item 3" ]) /\
        """- Item 1
- Item 2
- Item 3"""
    , (D.nest 0 $ D.stack
        [ D.mark "*" $ D.text "Item 1"
        , (D.mark "*" $ D.text "Item 2")
            <> D.break
            <> D.nest 1
                (D.stack
                    [ D.mark "-" $ D.text "Item 2.1"
                    , D.mark "-" $ D.text "Item 2.2"
                    , D.mark "-" $ D.text "Item 2.3"
                        <> D.break
                        <> D.nest 2 (D.mark "o" $ D.text "Item 2.3.1")
                    , D.mark "-" $ D.text "Item 2.4"
                    ])
        , D.mark "*" $ D.text "Item 3"
        ]) /\ """* Item 1
* Item 2
<-->- Item 2.1
<-->- Item 2.2
<-->- Item 2.3
<--><-->o Item 2.3.1
<-->- Item 2.4
* Item 3"""
    , (D.nest 0
            $ D.stack [ D.text "zero"
                       , D.nest 1
                            $ D.stack [ D.text "one"
                                       , D.nest 2
                                            $ D.stack [ D.text "two"
                                                       , D.nest 3
                                                            $ D.stack [ D.text "three"
                                                                       , D.nest 4 $ D.text "four"
                                                                       ]
                                                       ]
                                       ]
                       ])
        /\ """zero
<-->one
<--><-->two
<--><--><-->three
<--><--><--><-->four"""
    ,(D.nest 1 $ D.stack
        [ D.text "name" <+> D.text "["
        , D.nest 2 $ D.stack [ D.text "a", D.text "b", D.text "c" ]
        , D.text "]"
        ]) /\ """<-->name [
<--><-->a
<--><-->b
<--><-->c
<-->]"""
    , showTree 0 tree
        /\ """aaa [
<-->bbb [
<--><-->ccc []
<--><-->dd []
<-->]
<-->eee []
<-->fff [
<--><-->gg []
<--><-->hhh []
<--><-->ii []
<-->]
]"""
    , showXML xml /\ ""
    ]

-- zero\n    one\n            two\n                    three\n                            four
-- zero\n    one\n        two\n            three\n                four


blessedSamples :: Array (F.Tag /\ String)
blessedSamples =
    [ F.s "foo" /\ "foo"
    , F.bolds "test" /\ "{bold}test{/bold}"
    ]


spec :: Spec Unit
spec = do

  describe "Formatting works properly for Doc" $ do

    -- helper { title : \idx (doc /\ _) -> show idx <> " : " <> show doc, render : D.layout 0 } docIndentedSamples
    helper { title : \idx (doc /\ _) -> show idx <> " : " <> String.take 200 (show doc), render : D.render } docIndentedSamples

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


showTree :: Int -> Tree -> D.Doc
showTree _ (Node s List.Nil) = D.text s <+> D.bracket "[" D.nil "]"
-- showTree n (Node s ts) = D.text s <+> D.text "[" </> (D.nest n $ D.stack $ Array.fromFoldable $ showTree (n + 1) <$> ts) </> D.nest (max 0 $ n - 1) (D.text "]")
showTree n (Node s ts) =
    D.nest n $ D.stack
        [ D.text s <+> D.text "["
        , D.nest (n + 1) $ D.stack $ Array.fromFoldable $ showTree (n + 1) <$> ts
        , D.text "]"
        ]

--     name [\n        a\n        b\n        c\n    ]
--     name [\n        a\n        b\n        c\n    ]

-- aaa [\n    bbb [\n        ccc []\n        dd []\n]\n    eee []\n    fff [\n        gg []\n        hhh []\n        ii []\n]\n]
-- aaa [\n    bbb [\n        ccc []\n        dd []\n    ]\n    eee []\n    fff [\n        gg []\n        hhh []\n        ii []\n    ]\n]


{-
showBracket :: List Tree -> D.Doc
showBracket List.Nil = D.nil
showBracket ts = D.text "[" <> D.nest 1 (showTrees ts) <> D.text "]"


showTrees :: List Tree -> D.Doc
showTrees List.Nil = D.nil
showTrees (List.Cons t List.Nil) = showTree t
showTrees (List.Cons t ts) = showTree t <> D.text "," <> D.break <> showTrees ts


showTree' :: Tree -> D.Doc
showTree' (Node s ts) = D.text s <> showBracket' ts


showBracket' :: List Tree -> D.Doc
showBracket' List.Nil = D.nil
showBracket' ts = D.bracket "[" (showTrees' ts) "]"


showTrees' :: List Tree -> D.Doc
showTrees' List.Nil = D.nil
showTrees' (List.Cons t List.Nil) = showTree' t
showTrees' (List.Cons t ts) = showTree' t <> D.text "," <> D.break <> showTrees' ts
-}


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


-- testtree :: Int -> String
-- testtree w = D.pretty w $ showTree tree
-- testtree' :: Int -> String
-- testtree' w = D.pretty w $ showTree' tree


data XML
    = Elt String (Array Att) (Array XML)
    | Txt String


data Att = Att String String


showXML :: XML -> D.Doc
showXML x = showXMLs 0 x -- D.folddoc (<>) (showXMLs x)


showXMLs :: Int -> XML -> D.Doc
showXMLs i (Elt n a []) =
    D.nest i
        $ D.stack
            [ D.text "<" <+> D.text n
            , D.nest (i + 1) $ D.stack $ showAttr <$> a
            , D.text "/>"
            ]
showXMLs i (Elt n a c)  =
    D.nest i
        $ D.stack
            $ [ D.text $ "<" <> n
              , D.nest (i + 1) $ D.stack $ showAttr <$> a
              , D.text ">"
              , D.folddoc (<>) (showXMLs (i + 1) <$> c)
              , D.bracket "<" (D.text n) "/>"
              ]
showXMLs i (Txt s) = D.text s

-- showTag :: String -> Array Att -> D.Doc
-- showTag n a = D.text n <> showFill showAtts a


showAttr :: Att -> D.Doc
showAttr (Att n v) = D.text n <> D.text "=" <> D.text (quoted v)


-- showAtts :: Att -> Array D.Doc
-- showAtts (Att n v) = [ D.text n <> D.text "=" <> D.text (quoted v) ]


quoted :: String -> String
quoted s = "\"" <> s <> "\""

-- showTag :: String -> Array Att -> D.Doc
-- showTag n a = D.text n <> showFill showAtts a

-- showFill :: forall a. (a -> Array D.Doc) -> Array a -> D.Doc
-- showFill _ [] = D.nil
-- showFill f xs = D.bracket "" (D.stack $ Array.concat $ map f xs) ""


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


-- testXML :: Int -> String
-- testXML w = D.pretty w $ showXML xml