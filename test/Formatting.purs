module Test.Formatting where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Text.Output.Blessed (render) as T
import Data.Text.Format as F
import Data.Text.Doc (Doc(..))
import Data.Text.Doc as D

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


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
    ]


blessedSamples :: Array (F.Tag /\ String)
blessedSamples =
    [ F.s "foo" /\ "foo"
    , F.bolds "test" /\ "{bold}test{/bold}"
    ]


spec :: Spec Unit
spec = do

  describe "Formatting works properly for Doc" $ do

    helper { title : \idx (doc /\ _) -> show idx <> " : " <> show doc, render : D.layout 0 } docIndentedSamples

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