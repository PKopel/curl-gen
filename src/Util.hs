{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( secondM
  ) where
import           RIO                            ( (<$>)
                                                , Functor
                                                )

secondM :: Functor m => (b -> m b') -> (a, b) -> m (a, b')
secondM f ~(a, b) = (a, ) <$> f b
