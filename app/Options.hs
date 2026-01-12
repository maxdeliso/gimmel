module Options (Options (..), IPVersion (..), optParser) where

import Options.Applicative (
  Parser,
  auto,
  flag',
  help,
  long,
  metavar,
  option,
  short,
  strOption,
  switch,
  value,
  (<|>),
 )

data IPVersion = IPv4 | IPv6 | Both
  deriving (Eq, Show)

data Options = MkOptions
  { portNumOpt :: Int
  , msgSizeOpt :: Int
  , ipVersionOpt :: IPVersion
  , logFileOpt :: String
  , beaconIntervalOpt :: Int
  , beaconEnabledOpt :: Bool
  }

optParser :: Parser Options
optParser =
  MkOptions
    <$> option
      auto
      ( long "portnum"
          <> short 'p'
          <> metavar "PORT"
          <> value 1337
          <> help "Listen on UDP PORT with specified number"
      )
    <*> option
      auto
      ( long "msglen"
          <> short 'M'
          <> metavar "MSGLEN"
          <> value 4096
          <> help "Accept messages of at most MSGLEN bytes"
      )
    <*> ( flag'
            IPv4
            ( long "4"
                <> short '4'
                <> help "Use IPv4 only"
            )
            <|> flag'
              IPv6
              ( long "6"
                  <> short '6'
                  <> help "Use IPv6 only (default)"
              )
            <|> pure IPv6 -- Default to IPv6
        )
    <*> strOption
      ( long "logfile"
          <> short 'l'
          <> metavar "FILE"
          <> value "server.log"
          <> help "Write logs to FILE (default: server.log)"
      )
    <*> option
      auto
      ( long "beacon-interval"
          <> metavar "SECONDS"
          <> value 5
          <> help "Send beacon every SECONDS (default: 5)"
      )
    <*> switch
      ( long "disable-beacon"
          <> help "Disable LAN peer discovery via beaconing"
      )