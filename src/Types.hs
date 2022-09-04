{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( App(..)
  , Options(..)
  , Curl(..)
  , Header(..)
  , Option
  , Dta
  , URL(..)
  ) where

import           Data.Semigroup                 ( )
import           Data.Text                     as T
                                         hiding ( intercalate
                                                , map
                                                )
import           RIO                            ( ($)
                                                , (<>)
                                                , Bool
                                                , HasLogFunc(..)
                                                , LogFunc
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

type Option = T.Text

data URL = URL
  { protocol :: T.Text
  , host     :: T.Text
  , path     :: T.Text
  }

type Dta = T.Text

newtype Header = H T.Text

data Curl = Curl URL [Option] [Header] Dta

instance Show Header where
  show (H h) = T.unpack $ "--header " <> h

instance Show URL where
  show (URL p h a) = T.unpack $ p <> "://" <> h <> a

instance Show Curl where
  show (Curl url ops hds dta) = intercalate
    " \\\n\t"
    (fstLine : show url : map show hds)
   where
    fstLine = T.unpack $ T.unwords ("curl" : "--data '" <> dta <> "'" : ops)
