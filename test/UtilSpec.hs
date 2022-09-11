{-# LANGUAGE NoImplicitPrelude #-}
module UtilSpec
  ( spec
  ) where

import           RIO                            ( ($)
                                                , Bool(..)
                                                , Eq(..)
                                                , Num(..)
                                                )
import           RIO.Text                       ( any
                                                , length
                                                )
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Util

spec :: Spec
spec = do
  describe "Util.indent" $ do
    prop "contents" $ \i -> any (/= ' ') (indent i) `shouldBe` False
    prop "length"
      $ \i -> let pi = abs i in length (indent pi) `shouldBe` 4 * pi
