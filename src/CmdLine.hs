module CmdLine(applicationOptionsParser) where

import Options.Applicative (Parser, ParserInfo, strOption, long,
    short, metavar, value, showDefault, help, auto, option, info,
    (<**>), helper, fullDesc, progDesc)
import Data.Semigroup ((<>))

import Types (ApplicationOptions(ApplicationOptions))

parseOptions :: Parser ApplicationOptions
parseOptions = ApplicationOptions
    <$> strOption
        ( long "database"
          <> short 'f'
          <> metavar "DATABASE"
          <> showDefault
          <> value "devices.db"
          <> help "SQLite DB path" )
    <*> option auto
        ( long "webserver-port"
          <> short 'p'
          <> help "Port to run the webserver"
          <> showDefault
          <> value 3000
          <> metavar "INT" )

applicationOptionsParser :: ParserInfo ApplicationOptions
applicationOptionsParser = info
    (parseOptions <**> helper)
    (fullDesc <> progDesc "Basic device registry database webapp")
