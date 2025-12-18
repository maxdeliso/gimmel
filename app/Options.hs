module Options(Options, portNumOpt, msgSizeOpt, ipVersionOpt, IPVersion(..), optParser) where

import Options.Applicative
    ( auto, help, long, metavar, option, short, switch, value, flag', Parser, (<|>) )

data IPVersion = IPv4 | IPv6 | Both
  deriving (Eq, Show)

data Options = MkOptions
  { portNumOpt :: Int
  , msgSizeOpt :: Int
  , ipVersionOpt :: IPVersion
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
  <*> ( flag' IPv4
          ( long "4"
         <> short '4'
         <> help "Use IPv4 only" )
     <|> flag' IPv6
          ( long "6"
         <> short '6'
         <> help "Use IPv6 only (default)" )
     <|> pure IPv6 )  -- Default to IPv6
