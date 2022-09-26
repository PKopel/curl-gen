{-# LANGUAGE NoImplicitPrelude #-}

module App
    ( App(..)
    , Options(..)
    , ScriptOptions(..)
    , ScriptLang(..)
    ) where

import           RIO                            ( Bool
                                                , HasLogFunc(..)
                                                , LogFunc
                                                , Read
                                                , Show
                                                , String
                                                , lens
                                                )
import           RIO.Process                    ( HasProcessContext(..)
                                                , ProcessContext
                                                )


-- | Command line arguments
data Options = Options
    { filePath      :: !String
    , scriptOptions :: !ScriptOptions
    , verbose       :: !Bool
    , outputPath    :: !String
    }

data ScriptLang = Bash | Powershell | OsDefault deriving (Show, Read)

data ScriptOptions = ScriptOptions
    { threads :: !Bool
    , random  :: !Bool
    , lang    :: !ScriptLang
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
