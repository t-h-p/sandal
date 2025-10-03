module Settings
  (
    Settings(..),
    getSettings
  ) where

import Options.Applicative

data Settings = Settings
  {
    inputDir :: String
  , outputF :: Maybe String
  , squareDirs :: Bool
  , colorize :: Bool
  , showAll :: Bool
  } deriving (Show)

inputDirParser :: Parser String
inputDirParser =
  strArgument
    (metavar "INPUT" <> help "Input directory")

outputFParser :: Parser (Maybe String)
outputFParser =
  optional $ strOption
    (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file")

squareDirsParser :: Parser Bool
squareDirsParser =
  switch (long "square-dirs" <> short 's' <> help "Make directories square")

colorizeParser :: Parser Bool
colorizeParser =
  switch (long "colorize" <> short 'c' <> help "Colorize output")

showAllParser :: Parser Bool
showAllParser =
  switch (long "showall" <> short 'S' <> help "Show all files")

settingsParser :: Parser Settings
settingsParser = Settings
  <$> inputDirParser
  <*> outputFParser
  <*> squareDirsParser
  <*> colorizeParser
  <*> showAllParser

parserInfo :: ParserInfo Settings
parserInfo = info (settingsParser <**> helper)
  (fullDesc <> progDesc "Generate graph rooted at INPUT" <> header "sandal: graph my file system")

getSettings :: IO Settings
getSettings = execParser parserInfo