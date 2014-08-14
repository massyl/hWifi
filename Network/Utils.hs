module Network.Utils where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Utils
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  unportable
-- Dependency  :
--
-- Utility functions module
--
-----------------------------------------------------------------------------

import qualified Data.Text as T
import System.Process (readProcess)
import Data.List (delete, isPrefixOf)
import Data.Functor((<$>))
import Control.Exception (catch, SomeException(..))
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO(stderr, hFlush, hPrint)
import Network.Types (ThrowsError, CommandError(..))

-- | Split string s on `sep` string
split :: String -> String -> [String]
split sep s = map T.unpack $ T.splitOn (T.pack sep) (T.pack s)

-- | Runs a command and returns the output as a string list
run :: String -> IO (ThrowsError [String])
run c@[]    = return $ Left $ BadCommand c
run command = (return . lines <$> readProcess comm args []) `catchIO` (Left $ BadCommand command)
  where (comm:args) = split " " command

-- | Utility function to trim the ' in a string
clean :: Char -> String -> String
clean c cs = if [c] `isPrefixOf` cs then sanitize cs else cs
  where sanitize = delChar . reverse . delChar . reverse
        delChar  = delete c

-- | Format the message
formatMsg :: String -> (String -> String) -> ThrowsError [String] -> [String]
formatMsg prefix f inputs = case inputs of
  Left err  -> [show err]
  Right val -> (prefix :) . map f $ val

-- | Executes a given `IO a` action, catches and print to stderr any thrown
-- | exception, then returns a defValue and continues execution
catchIO :: MonadIO m => IO a -> a -> m a
catchIO ma defValue = liftIO (ma `Control.Exception.catch` \(SomeException e) ->
                      hPrint stderr e >> hFlush stderr >> return defValue)
