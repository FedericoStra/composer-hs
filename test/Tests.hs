module Main (main) where

import Composer
import qualified Control.Monad as Monad
import Data.Char (toUpper)
import qualified System.Exit as Exit
import Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT tests
  let hasErrors = errors counts /= 0
      hasFailures = failures counts /= 0
  Monad.when (hasErrors || hasFailures) Exit.exitFailure

tests =
  TestList
    [ -- simple composition
      "comp Empty" ~: testCompEmpty comp,
      "comp' Empty" ~: testCompEmpty comp',
      "comp Int" ~: testCompInt comp,
      "comp String" ~: testCompString comp,
      "comp' Int" ~: testCompInt comp',
      "comp' String" ~: testCompString comp',
      "compRev Empty" ~: testCompRevEmpty compRev,
      "compRev' Empty" ~: testCompRevEmpty compRev',
      "compRev Int" ~: testCompRevInt compRev,
      "compRev String" ~: testCompRevString compRev,
      "compRev' Int" ~: testCompRevInt compRev',
      "compRev' String" ~: testCompRevString compRev'
      -- parameterized composition
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
