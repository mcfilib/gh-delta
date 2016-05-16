{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.String         (fromString)
import qualified GitHub              as GH
import           Lib                 (DeltaParams (..), generate)
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long,
                                      progDesc, strOption, (<>))

data CLIOpts = CLIOpts { auth :: String, owner :: String, repo :: String, since :: String }

cliOpts :: ParserInfo CLIOpts
cliOpts =
  info (helper <*> cliOptsParser)
    (fullDesc
     <> progDesc "Simple, opinionated, Github changelog generator written in Haskell"
     <> header "gh-delta - changelog generator")

cliOptsParser :: Parser CLIOpts
cliOptsParser =
  CLIOpts <$> strOption (long "auth"
                         <> help "Personal access token")
          <*> strOption (long "owner"
                         <> help "Repository owner")
          <*> strOption (long "repo"
                         <> help "Repository name")
          <*> strOption (long "since"
                         <> help "Since SHA")

runCli :: CLIOpts -> IO ()
runCli CLIOpts { .. } = generate params
  where
    params = DeltaParams (GH.OAuth . fromString $ auth) (fromString owner) (fromString repo)
               (fromString since)

main :: IO ()
main = execParser cliOpts >>= runCli
