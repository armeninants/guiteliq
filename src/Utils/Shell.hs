{-
Module      : Utils.Shell
Description : Utilities for external processes
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Utilities for external processes.
-}
module Utils.Shell where

import RIO
import System.Process.Text (readProcessWithExitCode)

-- | Run a shell command.
runCommand :: String -> [String] -> IO (Either Text Text)
runCommand cmd args = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> return $ Right stdOut
    _ -> return $ Left stdErr

runCommand_ :: String -> [String] -> IO ()
runCommand_ cmd args = void $ readProcessWithExitCode cmd args ""
