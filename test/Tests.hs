module Main (main) where

import Composer
import Control.Monad (when)
import Data.Char (toUpper)
import System.Exit (exitFailure)
import Test.HUnit hiding (counts)

main :: IO ()
main = do
  counts <- runTestTT tests
  let hasErrors = errors counts /= 0
      hasFailures = failures counts /= 0
  when (hasErrors || hasFailures) exitFailure

tests =
  TestList
    [ -- simple composition
      "comp Empty" ~: testCompEmpty comp,
      "comp' Empty" ~: testCompEmpty comp',
      "comp Int" ~: testCompInt comp,
      "comp String" ~: testCompString comp,
      "comp' Int" ~: testCompInt comp',
      "comp' String" ~: testCompString comp',
      "comp Infinite" ~: testCompInfinite,
      "compRev Empty" ~: testCompRevEmpty compRev,
      "compRev' Empty" ~: testCompRevEmpty compRev',
      "compRev Int" ~: testCompRevInt compRev,
      "compRev String" ~: testCompRevString compRev,
      "compRev' Int" ~: testCompRevInt compRev',
      "compRev' String" ~: testCompRevString compRev',
      -- parameterized composition
      "compMap Int" ~: testCompMapInt compMap,
      "compMap_ Int" ~: testCompMap_Int compMap_,
      "compMap Infinite" ~: testCompMapInfinite,
      "compMapRev' Int" ~: testCompMapRevInt compMapRev',
      "compMapRev_' Int" ~: testCompMapRev_Int compMapRev_'
    ]

-- `comp` and `comp'`

testCompEmpty cmp =
  "empty" ~: cmp [] 42 ~?= 42

testCompInt cmp =
  TestList
    [ "2x+1" ~: cmp [(+ 1), (* 2)] 3 ~?= 7,
      "2(x+1)" ~: cmp [(* 2), (+ 1)] 3 ~?= 8
    ]

testCompString cmp =
  TestList
    [ "hello world" ~: cmp [("Hello " ++), (++ "!")] "World" ~?= "Hello World!",
      "string" ~: cmp [\s -> "zz" ++ s ++ "zz", map toUpper] "warning" ~?= "zzWARNINGzz"
    ]

testCompInfinite =
  TestList
    [ "numbers" ~: (take 20 $ comp (map ((++) . show) [0 ..]) ".") ~?= "01234567891011121314"
    ]

-- `compRev` and `compRev'`

testCompRevEmpty cmp =
  "empty" ~: cmp [] 42 ~?= 42

testCompRevInt cmp =
  TestList
    [ "2(x+1)" ~: cmp [(+ 1), (* 2)] 3 ~?= 8,
      "2x+1" ~: cmp [(* 2), (+ 1)] 3 ~?= 7
    ]

testCompRevString cmp =
  TestList
    [ "hello world" ~: cmp [("Hello " ++), (++ "!")] "World" ~?= "Hello World!",
      "string" ~: cmp [\s -> "zz" ++ s ++ "zz", map toUpper] "warning" ~?= "ZZWARNINGZZ"
    ]

-- `compMap` and `compMap_`

testCompMapInt cmpM =
  TestList
    [ "1+(2+(3+4))" ~: cmpM (+) [1, 2, 3] 4 ~?= 1 + 2 + 3 + 4,
      "2^(3^4)" ~: cmpM (^) [2, 3] 4 ~?= 2 ^ (3 ^ 4)
    ]

testCompMapInfinite =
  TestList
    [ "numbers" ~: (take 20 $ compMap ((++) . show) [0 ..] ".") ~?= "01234567891011121314"
    ]

testCompMapRevInt cmpM =
  TestList
    [ "((4+1)+2)+3" ~: cmpM (+) [1, 2, 3] 4 ~?= 1 + 2 + 3 + 4,
      "3^(2^4)" ~: cmpM (^) [2, 3] 4 ~?= 3 ^ 2 ^ 4
    ]

testCompMap_Int cmpM =
  TestList
    [ "1+(2+(3+(4+10))" ~: cmpM (+) (+ 10) [1, 2, 3] 4 ~?= 1 + 2 + 3 + 4 + 10,
      "2^(3^4)" ~: cmpM (^) (+ 1) [2, 3] 3 ~?= 2 ^ (3 ^ 4)
    ]

testCompMapRev_Int cmpM =
  TestList
    [ "(((4+10)+1)+2)+3" ~: cmpM (+) (+ 10) [1, 2, 3] 4 ~?= 1 + 2 + 3 + 4 + 10,
      "3^(2^4)" ~: cmpM (^) (+ 1) [2, 3] 3 ~?= 3 ^ 2 ^ 4
    ]
