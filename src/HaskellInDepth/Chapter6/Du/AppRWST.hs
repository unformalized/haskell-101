{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module HaskellInDepth.Chapter6.Du.AppRWST where

import Control.Monad.RWS (RWST, evalRWST)
import HaskellInDepth.Chapter6.Du.AppTypes (AppConfig, AppEnv, initialEnv)

type MyApp logEntry state =
  RWST
    AppEnv -- read
    [logEntry] -- write
    state -- state
    IO -- base Monad

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config st = evalRWST app (initialEnv config) st
