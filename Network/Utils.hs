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
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/)
--
-- A simple module to deal with wifi connections.
-- At the moment, only election of the wifi with the most powerful signal and autoconnect policy.
--
-- Use: runhaskell Network/HWifi.hs
-----------------------------------------------------------------------------

import System.Process
import Data.List (delete, isPrefixOf)
import Control.Monad.Error
import Control.Exception
import System.IO

data CommandError = EmptyCommand | InvalidCommand | OtherError String deriving (Show, Eq)
instance Error(CommandError) where
  noMsg = OtherError "Some problem occured during command execution"
  strMsg = OtherError

type ProcessMonad = ErrorT CommandError IO

runProcessMonad:: ProcessMonad a -> IO (Either CommandError a)
runProcessMonad = runErrorT

-- | Run a command and displays the output in list of strings
run :: String -> IO [String]
run []      = return []
run command = (readProcess comm args [] >>= return . lines) `catchIO` []
  where (comm:args) = words command

-- | Utility function to trim the ' in a string
clean :: Char -> String -> String
clean c cs = if isPrefixOf [c] cs then sanitize cs else cs
  where sanitize = delChar . reverse . delChar . reverse
        delChar  = delete c

logMsg :: String -> (String -> String) -> [String] -> [String]
logMsg prefix f = (prefix :) . map f

-- | executes a given `IO a` action, catches and print to stderr any thrown
-- | exception, then return a defValue and continue execution
catchIO :: MonadIO m => IO a -> a -> m a
catchIO ma defValue = liftIO (ma `catch` \(SomeException e) ->
                      hPrint stderr e >> hFlush stderr >> return defValue)
