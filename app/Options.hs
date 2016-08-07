module Options(Options, portNumOpt, msgSizeOpt, optParser) where

import Options.Applicative
    ( auto, help, long, metavar, option, short, switch, value, Parser )

data Options = MkOptions
  { portNumOpt :: Int
  , msgSizeOpt :: Int
  }

optParser :: Parser Options
optParser = MkOptions
  <$> option auto
      ( long "portnum"
     <> short 'p'
     <> metavar "PORT"
     <> value 1337
     <> help "Listen on UDP PORT with specified number" )
  <*> option auto
      ( long "msglen"
     <> short 'M'
     <> metavar "MSGLEN"
     <> value 4096
     <> help "Accept messages of at most MSGLEN bytes" )
