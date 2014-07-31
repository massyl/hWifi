module Network.Utils where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Utils
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  portable
-- Dependency  :
--
-- Utility functions module
--
-----------------------------------------------------------------------------

import System.Process (readProcess)
import Data.List (delete, isPrefixOf)
import Data.Functor((<$>))
import Control.Exception (catch, SomeException(..))
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO(stderr, hFlush, hPrint)

-- | Runs a command and displays the output as a string list
run :: String -> IO [String]
run []      = return []
run command = lines <$> readProcess comm args [] `catchIO` []
  where (comm:args) = words command

-- | Utility function to trim the ' in a string
clean :: Char -> String -> String
clean c cs = if [c] `isPrefixOf` cs then sanitize cs else cs
  where sanitize = delChar . reverse . delChar . reverse
        delChar  = delete c

-- | TODO rename this function to more relevant name
logMsg :: String -> (String -> String) -> [String] -> [String]
logMsg prefix f = (prefix :) . map f

-- | Executes a given `IO a` action, catches and print to stderr any thrown
-- | exception, then returns a defValue and continues execution
catchIO :: MonadIO m => IO a -> a -> m a
catchIO ma defValue = liftIO (ma `catch` \(SomeException e) ->
                      hPrint stderr e >> hFlush stderr >> return defValue)
