{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Framework                  (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import           Control.Monad.IO.Class          (liftIO)
import           Graphics.ImageMagick.MagickWand

main = defaultMain tests

-- tests mostly taken from wand(http://dahlia.kr/wand/) source code
tests =
    [
      testGroup "Behaves to spec"
      [
        testCase "test 1" test_1
      ]
    ]


test_1 = withImage "mona-lisa.jpg" $ \w -> do
  liftIO $ 1 @=? 1

withImage name f = withMagickWandGenesis $ do
  (_,w) <- magickWand
  readImage w name
  f w

