module Main where

import qualified Hebele.Hubele.Server.Cli as Cli
import System.Exit (exitWith)


main :: IO ()
main = Cli.runCli >>= exitWith
