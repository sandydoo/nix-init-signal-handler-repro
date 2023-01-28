module Main where

import qualified Hercules.CNix as CNix
import qualified Hercules.CNix.Util as CNix.Util
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  CNix.init
  CNix.Util.installDefaultSigINTHandler

  print "Initialized Nix store"

  -- Wait for 10 seconds
  threadDelay (10 * 1000 * 1000)

  print "Exiting"
