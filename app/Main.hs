{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.String         (fromString)
import qualified GitHub              as GH
import           Lib                 (DeltaParams (..), generate)
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long,
                                      optional, progDesc, strOption, (<>))
import           System.Environment  (lookupEnv)

data CLIOpts = CLIOpts { auth :: Maybe String, owner :: String, repo :: String, since :: String }

cliOpts :: ParserInfo CLIOpts
cliOpts =
  info (helper <*> cliOptsParser)
    (fullDesc
     <> progDesc "Simple, opinionated, Github changelog generator written in Haskell"
     <> header "gh-delta - changelog generator")

cliOptsParser :: Parser CLIOpts
cliOptsParser =
  CLIOpts <$> optional (strOption (long "auth"
                                   <> help "Personal access token"))
          <*> strOption (long "owner"
                         <> help "Repository owner")
          <*> strOption (long "repo"
                         <> help "Repository name")
          <*> strOption (long "since"
                         <> help "Since SHA")

runCli :: CLIOpts -> IO ()
runCli CLIOpts { .. } = do
  envAuth <- lookupEnv "GH_DELTA_AUTH"
  generate $
    case (envAuth, auth) of
      (Just x, _) -> params x
      (_, Just x) -> params x
      _           -> error "set GH_DELTA_AUTH environment variable or specify --auth"

  where
    params authToken = DeltaParams
                         (GH.OAuth . fromString $ authToken)
                         (fromString owner)
                         (fromString repo)
                         (fromString since)

main :: IO ()
main = execParser cliOpts >>= runCli
