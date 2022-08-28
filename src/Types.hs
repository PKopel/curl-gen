{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types
  ( App (..)
  , Options (..)
  , Curl (..)
  , Header (..)
  ) where

import RIO ( Bool, lens, HasLogFunc(..), LogFunc, (<>), Show (show), map, ($))
import RIO.Process
import qualified Data.Text.Lazy                as Lazy
import RIO.List (intercalate)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

type Option = Lazy.Text
type URL = Lazy.Text
type Data = Lazy.Text

newtype Header = H Lazy.Text
data Curl = Curl [Option] URL [Header] Data

instance Show Header where
  show (H h) = Lazy.unpack $ "--header " <> h

instance Show Curl where
  show (Curl ops url hds dta) =  intercalate " \\\n\t" (fstLine: map show hds)
    where
      fstLine = Lazy.unpack $ Lazy.unwords ("curl": url:"--data " <> dta:ops)
