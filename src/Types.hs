{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( App(..)
  , Options(..)
  , Argument(..)
  , Curl(..)
  , Header(..)
  , URL(..)
  , Option
  , Dta
  ) where

import           Data.Semigroup                 ( )
import           Data.Text                     as T
                                         hiding ( intercalate
                                                , map
                                                )
import           RIO                            ( ($)
                                                , (<>)
                                                , Bool(..)
                                                , Eq
                                                , HasLogFunc(..)
                                                , LogFunc
                                                , Ord(..)
                                                , Show(show)
                                                , lens
                                                , map
                                                )
import           RIO.List                       ( intercalate )
import           RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  }

    -- Add other app-specific configuration information here
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })


data Argument = A URL | P T.Text T.Text | F  T.Text deriving (Show, Eq, Ord)

type Option = T.Text


newtype Dta = D T.Text
instance Show Dta where
  show (D h) = T.unpack $ "--data '" <> h <> "'"

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

data Curl = Curl URL [Option] [Header] Dta

instance Show Curl where
  show (Curl url ops hds dta) = intercalate
    " \\\n\t"
    (fstLine : show url : show dta : map show hds)
    where fstLine = T.unpack $ T.unwords ("curl" : ops)
