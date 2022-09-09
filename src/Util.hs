{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Util
  ( secondM
  ) where
import           RIO                            ( (<$>)
                                                , Functor
                                                )

secondM :: Functor m => (b -> m b') -> (a, b) -> m (a, b')
secondM f ~(a, b) = (a, ) <$> f b
