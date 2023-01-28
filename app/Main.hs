{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Text (Text)
import qualified Hercules.CNix as CNix
import qualified Hercules.CNix.Util as CNix.Util
import Protolude
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)
import System.Posix (Handler (Catch), installHandler, sigHUP, sigINT, sigTERM, sigUSR1)

main :: IO ()
main = do
  printBlockedSignals

  putLText "Running initNix"
  CNix.init

  printBlockedSignals

  putLText "Installing CNix handler"
  installDefaultSigINTHandler

  printBlockedSignals

  -- let signalset
  --       = emptySignalSet
  --       & addSignal sigINT
  --       & addSignal sigTERM
  --       & addSignal sigHUP
  -- putLText "Unblocking signals hack"
  -- unblockSignals signalset

  printBlockedSignals

  putLText "Waiting for user interrupt"

  -- Wait for 10 seconds
  threadDelay (10 * 1000 * 1000)

  print "Exiting"

printBlockedSignals :: IO ()
printBlockedSignals = do
  putLText "Blocked signals"
  mask <- getSignalMask 
  print $ map (isBlocked mask) [sigINT, sigTERM, sigHUP]
  putLText ""
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

installDefaultSigINTHandler :: IO ()
installDefaultSigINTHandler = do
  mainThread <- myThreadId
  weakId <- mkWeakThreadId mainThread
  let defaultHaskellHandler = do
        mt <- deRefWeak weakId
        for_ mt \t -> do
          throwTo t (toException UserInterrupt)

  -- Install Nix interrupter in Haskell
  _oldHandler <-
    for [sigINT, sigTERM, sigHUP] \sig ->
      installHandler
        sig
        ( Catch do
            putLText "Interrupt!"
            CNix.Util.triggerInterrupt
            defaultHaskellHandler
        )
        Nothing

  -- Install dummy SIGUSR1 handler for Nix interrupt signal propagation
  -- (installHandler uses process-wide sigprocmask, so this should apply to all
  -- capability threads, as required for Nix)
  _oldHandler <-
    installHandler
      sigUSR1
      ( -- Not Ignore, because we want to cause EINTR
        Catch (putLText "sigUSR1" >> pass)
      )
      Nothing

  -- Install Haskell interrupter in Nix
  CNix.Util.createInterruptCallback defaultHaskellHandler
