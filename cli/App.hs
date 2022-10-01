{-# LANGUAGE NoImplicitPrelude #-}

module App
    ( App(..)
    , Options(..)
    , module Types.Script
    ) where

import           RIO                            ( Bool
                                                , HasLogFunc(..)
                                                , LogFunc
                                                , String
                                                , lens
                                                )
import           RIO.Process                    ( HasProcessContext(..)
                                                , ProcessContext
                                                )
import           Types.Script                   ( ScriptOptions(..) )


-- | Command line arguments
data Options = Options
    { filePath      :: !String
    , scriptOptions :: !ScriptOptions
    , verbose       :: !Bool
    , outputPath    :: !String
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
