module Tests.Identities (testIdentities) where

import Composer
import Data.Char (toUpper)
import Data.Foldable (fold)
import Data.Monoid (Endo (Endo, appEndo))
import Test.HUnit

testIdentities = TestList ["comp" ~: testComp, "compMap" ~: testCompMap]

-- forms equivalent to 'comp'
testComp =
  TestList [call fns input ~?= output | call <- invocations]
  where
    fns = [(++ "!"), ("say:" ++), map toUpper, (" " ++)]
    input = "hello"
    output = "say: HELLO!"
    invocations =
      [ comp,
        (compRev . reverse),
        compMap id,
        appEndo . foldMap Endo,
        appEndo . fold . map Endo,
        appEndo . mconcat . map Endo,
        appEndo . foldMap id . map Endo
      ]

-- forms equivalent to 'compMap'
testCompMap =
  TestList [call list input ~?= output | call <- invocations]
  where
    f = flip (++)
    list = ["...", "!", "?"]
    input = "hello"
    output = "hello?!..."
    invocations =
      [ compMap f,
        (comp . map f),
        foldr (\x chain -> f x . chain) id,
        foldr ((.) . f) id,
        compMap_ f id,
        appEndo . foldMap (Endo . f),
        (appEndo .) . foldMap . (Endo .) $ f
      ]
