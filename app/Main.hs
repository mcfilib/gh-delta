{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative ((<|>))
import           Data.Function       ((&))
import           Lib                 (DeltaParams, defaultDeltaParams, generate,
                                      setDeltaParamsAuth, setDeltaParamsOwner,
                                      setDeltaParamsRepo, setDeltaParamsSince)
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long,
                                      optional, progDesc, strOption, (<>))
import           System.Environment  (lookupEnv)

data CLIOpts =
       CLIOpts
         { cliAuth  :: Maybe String
         , cliOwner :: String
         , cliRepo  :: String
         , cliSince :: String
         }

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
  generate $ params (envAuth <|> cliAuth)

  where
    params :: Maybe String -> DeltaParams
    params mbAuthToken = defaultDeltaParams
                         & setDeltaParamsAuth mbAuthToken
                         & setDeltaParamsOwner cliOwner
                         & setDeltaParamsRepo cliRepo
                         & setDeltaParamsSince cliSince

main :: IO ()
main = execParser cliOpts >>= runCli
