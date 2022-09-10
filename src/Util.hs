{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Util
  ( secondM
  , indent
  ) where
import           RIO                            ( (<$>)
                                                , Functor
                                                , Int
                                                , Text
                                                )
import           RIO.Text                       ( replicate )

secondM :: Functor m => (b -> m b') -> (a, b) -> m (a, b')
secondM f ~(a, b) = (a, ) <$> f b

indent :: Int -> Text
indent n = replicate n "    "
