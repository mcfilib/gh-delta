{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative ((<|>))
import           Data.Function       ((&))
import qualified Data.Text.IO        as T
import           Lib                 (DeltaParams, defaultDeltaParams, generate,
                                      setDeltaParamsAuth, setDeltaParamsOwner,
                                      setDeltaParamsRepo, setDeltaParamsSince,
                                      setDeltaParamsUntil,
                                      setDeltaParamsVersion)
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long,
                                      optional, progDesc, strOption, (<>))
import           System.Environment  (lookupEnv)
import           System.Exit         (die)

data CLIOpts =
       CLIOpts
         { cliAuth    :: Maybe String
         , cliOwner   :: String
         , cliRepo    :: String
         , cliSince   :: String
         , cliUntil   :: Maybe String
         , cliVersion :: Maybe String
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
          <*> optional (strOption (long "until"
                                   <> help "Until SHA"))
          <*> optional (strOption (long "version"
                                   <> help "Version for changelog entry"))

runCli :: CLIOpts -> IO ()
runCli CLIOpts { .. } = do
  envAuth <- lookupEnv "GH_DELTA_AUTH"
  result <- generate $ params (envAuth <|> cliAuth)
  case result of
    Left err        -> die err
    Right changelog -> T.putStrLn changelog

  where
    params :: Maybe String -> DeltaParams
    params mbAuthToken = defaultDeltaParams
                         & setDeltaParamsAuth mbAuthToken
                         & setDeltaParamsOwner cliOwner
                         & setDeltaParamsRepo cliRepo
                         & setDeltaParamsSince cliSince
                         & setDeltaParamsUntil cliUntil
                         & setDeltaParamsVersion cliVersion

main :: IO ()
main = execParser cliOpts >>= runCli
