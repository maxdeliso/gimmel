module Main where

import Net
    ( netMain )
import Options
    ( Options, optParser )
import Options.Applicative
    ( fullDesc, info, progDesc, execParser, helper )

main :: IO ()
main = execParser opts >>= runWithOpts
  where
    opts = info (helper <*> optParser) (fullDesc <> progDesc "UDP chat server")

runWithOpts :: Options -> IO ()
runWithOpts = netMain
