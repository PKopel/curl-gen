{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Curl
    ( Argument(..)
    , Curl(..)
    , Header(..)
    , URL(..)
    , Option
    , Dta(..)
    ) where

import           Data.Semigroup                 ( )
import           RIO                            ( ($)
                                                , (<>)
                                                , Eq
                                                , Ord(..)
                                                , Show(show)
                                                , map
                                                )
import           RIO.List                       ( intercalate )
import           RIO.Text                      as T
                                         hiding ( intercalate
                                                , map
                                                )

data Argument = A URL | P T.Text T.Text | F  T.Text deriving (Show, Eq, Ord)

type Option = T.Text


data Dta = D T.Text | NoData
instance Show Dta where
    show (D h)  = T.unpack $ "--data '" <> h <> "'"
    show NoData = ""

newtype Header = H T.Text

instance Show Header where
    show (H h) = T.unpack $ "--header '" <> h <> "'"

data URL = URL
    { protocol :: T.Text
    , host     :: T.Text
    , path     :: T.Text
    }
    deriving (Eq, Ord)

instance Show URL where
    show (URL p h a) = T.unpack $ p <> "://" <> h <> a

data Curl = Curl
    { url :: URL
    , ops :: [Option]
    , hds :: [Header]
    , dta :: Dta
    }

instance Show Curl where
    show (Curl u o h d) = intercalate
        " \\\n\t"
        (fstLine : show u : show d : map show h)
        where fstLine = T.unpack $ T.unwords ("curl" : o)

