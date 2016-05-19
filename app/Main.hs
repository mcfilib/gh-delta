{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative ((<|>))
import           Data.Function       ((&))
import qualified Data.Text.IO        as T
import           Data.Version        (showVersion)
import           Lib                 (DeltaParams, defaultDeltaParams, generate,
                                      setDeltaParamsAuth, setDeltaParamsLabel,
                                      setDeltaParamsOwner, setDeltaParamsRepo,
                                      setDeltaParamsSince, setDeltaParamsUntil)
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, hidden, info,
                                      infoOption, long, optional, progDesc,
                                      short, strOption, (<>))
import           Paths_gh_delta      (version)
import           System.Environment  (lookupEnv)
import           System.Exit         (die)

data CLIOpts =
       CLIOpts
         { cliAuth  :: Maybe String
         , cliOwner :: String
         , cliRepo  :: String
         , cliSince :: String
         , cliUntil :: Maybe String
         , cliLabel :: Maybe String
         }

cliOpts :: ParserInfo CLIOpts
cliOpts =
  info (helper <*> cliVersion <*> cliOptsParser)
    (fullDesc
     <> progDesc "Simple, opinionated, Github changelog generator written in Haskell"
     <> header "gh-delta - changelog generator")

cliVersion :: Parser (a -> a)
cliVersion = infoOption (showVersion version)
               (long "version" <> short 'v' <> help "Show version information" <> hidden)

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
          <*> optional (strOption (long "label"
                                   <> help "Label for changelog entry"))

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
                         & setDeltaParamsLabel cliLabel

main :: IO ()
main = execParser cliOpts >>= runCli
