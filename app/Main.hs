{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Data.Function ((&))
import Data.Text (Text)
import qualified Hercules.CNix as CNix
import qualified Hercules.CNix.Util as CNix.Util
import System.Posix.Signals

main :: IO ()
main = do
  printBlockedSignals

  putStrLn "Running initNix"
  CNix.init

  printBlockedSignals

  putStrLn "Installing CNix handler"
  CNix.Util.installDefaultSigINTHandler

  printBlockedSignals

  -- putStrLn "Unblocking signals hack"
  -- unblockSignals
  --   $ emptySignalSet
  --   & addSignal sigINT
  --   & addSignal sigTERM
  --   & addSignal sigHUP

  printBlockedSignals

  putStrLn "Waiting for user interrupt"

  -- Wait for 10 seconds
  threadDelay (10 * 1000 * 1000)

  print "Exiting"

printBlockedSignals :: IO ()
printBlockedSignals = do
  putStrLn "Blocked signals"
  mask <- getSignalMask 
  print $ map (isBlocked mask) [sigINT, sigTERM, sigHUP]
  putStrLn ""
  where
    isBlocked :: SignalSet -> Signal -> (Text, Bool)
    isBlocked mask signal = do
      (showSignal signal, inSignalSet signal mask)

    showSignal :: Signal -> Text
    showSignal signal
      | signal == sigINT = "sigINT"
      | signal == sigTERM = "sigTERM"
      | signal == sigHUP = "sigHUP"
      | otherwise = undefined

