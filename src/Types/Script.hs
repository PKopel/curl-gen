{-# LANGUAGE NoImplicitPrelude #-}

module Types.Script
    ( ScriptOptions(..)
    , ScriptLang(..)
    , Generator
    ) where

import           RIO                            ( Bool
                                                , Read
                                                , Show
                                                , Text
                                                )
import           Types.Curl                     ( Curl )

type Generator = ([Text], Curl) -> Text

data ScriptLang = Bash | Powershell | OsDefault deriving (Show, Read)

data ScriptOptions = ScriptOptions
    { threads :: !Bool
    , random  :: !Bool
    , lang    :: !ScriptLang
    }
