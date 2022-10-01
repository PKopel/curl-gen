{-# LANGUAGE NoImplicitPrelude #-}

module Generators
    ( generators
    ) where

import           Bash.Function                 as BashF
                                                ( writeFunction )
import           Bash.Template                 as BashT
                                                ( script )
import           Powershell.Function           as PwshF
                                                ( writeFunction )
import           Powershell.Template           as PwshT
                                                ( script )
import           RIO                            ( Text )
import           System.Info                    ( os )
import           Types.Script                   ( Generator
                                                , ScriptLang(..)
                                                , ScriptOptions(..)
                                                )

generators :: ScriptOptions -> (Generator, [Text] -> Text)
generators opts = case lang opts of
    Bash       -> bash
    Powershell -> pwsh
    OsDefault  -> case os of
        "mingw32" -> pwsh
        _         -> bash
  where
    bash = (BashF.writeFunction (random opts), BashT.script opts)
    pwsh = (PwshF.writeFunction opts, PwshT.script opts)
