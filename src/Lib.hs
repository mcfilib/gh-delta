{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib (
    generate,
    DeltaParams,
    defaultDeltaParams,
    setDeltaParamsAuth,
    setDeltaParamsOwner,
    setDeltaParamsRepo,
    setDeltaParamsSince,
    setDeltaParamsUntil,
    setDeltaParamsVersion,
    ) where

import           Data.Function    ((&))
import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import           Data.String      (fromString)
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Data.Time.Clock  (UTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import qualified GitHub           as GH
import           System.Exit      (die)

-- | Parameters required to generate a Delta.
data DeltaParams =
       DeltaParams
         { deltaParamsAuth    :: Maybe GH.Auth
         , deltaParamsOwner   :: GH.Name GH.Owner
         , deltaParamsRepo    :: GH.Name GH.Repo
         , deltaParamsSince   :: GH.Name GH.GitCommit
         , deltaParamsUntil   :: Maybe (GH.Name GH.GitCommit)
         , deltaParamsVersion :: Maybe Text
         }

-- | Default params using the gh-delta repo.
defaultDeltaParams :: DeltaParams
defaultDeltaParams =
  DeltaParams Nothing "filib" "gh-delta" "f44caa05adf066ae441cbdbebe54010d94172e9a" Nothing Nothing

-- | Setter for personal access token.
setDeltaParamsAuth :: Maybe String -> DeltaParams -> DeltaParams
setDeltaParamsAuth x params = params { deltaParamsAuth = fmap (GH.OAuth . fromString) x }

-- | Setter for owner.
setDeltaParamsOwner :: String -> DeltaParams -> DeltaParams
setDeltaParamsOwner x params = params { deltaParamsOwner = fromString x }

-- | Setter for repo.
setDeltaParamsRepo :: String -> DeltaParams -> DeltaParams
setDeltaParamsRepo x params = params { deltaParamsRepo = fromString x }

-- | Setter for SHA since
setDeltaParamsSince :: String -> DeltaParams -> DeltaParams
setDeltaParamsSince x params = params { deltaParamsSince = fromString x }

-- | Setter for SHA until.
setDeltaParamsUntil :: Maybe String -> DeltaParams -> DeltaParams
setDeltaParamsUntil x params = params { deltaParamsUntil = fmap fromString x }

-- | Setter for version.
setDeltaParamsVersion :: Maybe String -> DeltaParams -> DeltaParams
setDeltaParamsVersion x params = params { deltaParamsVersion = fmap fromString x }

-- | Single event in a changelog.
data Event = Event { eventAuthor :: Text, eventTitle :: Text, eventLink :: Text }

-- | Single changelog entry.
data Delta =
       Delta
         { deltaDateSince :: Text
         , deltaDateUntil :: Text
         , deltaEvents    :: [Event]
         , deltaVersion   :: Maybe Text
         }

-- | Write changelog entry since SHA to STDOUT.
generate :: DeltaParams -> IO ()
generate params@DeltaParams { .. } = do
  dateSinceResponse <- commitDate params deltaParamsSince
  case dateSinceResponse of
    Left err -> renderError err
    Right dateSince ->
      case deltaParamsUntil of
        Just sha -> do
          dateUntilResponse <- commitDate params sha
          case dateUntilResponse of
            Left err -> renderError err
            Right dateUntil -> do
              pullRequests <- fetchPullRequests dateSince dateUntil
              renderTemplate dateSince dateUntil pullRequests
        Nothing -> do
          dateUntil <- getCurrentTime
          pullRequests <- fetchPullRequests dateSince dateUntil
          renderTemplate dateSince dateUntil pullRequests

  where
    fetchPullRequests :: UTCTime -> UTCTime -> IO (Vector GH.SimplePullRequest)
    fetchPullRequests = closedPullRequestsSince params

    renderError :: GH.Error -> IO ()
    renderError err = die $ show err

    renderTemplate :: UTCTime -> UTCTime -> Vector GH.SimplePullRequest -> IO ()
    renderTemplate x y z = T.putStrLn $ template (toDelta x y z deltaParamsVersion)

-- | Get date a commit was created.
commitDate :: DeltaParams -> GH.Name GH.GitCommit -> IO (Either GH.Error UTCTime)
commitDate DeltaParams { .. } sha = do
  response <- GH.executeRequestMaybe deltaParamsAuth $ GH.gitCommitR
                                                         deltaParamsOwner
                                                         deltaParamsRepo
                                                         sha
  case response of
    Left err     -> return $ Left err
    Right commit -> return $ Right (GH.gitUserDate $ GH.gitCommitAuthor commit)

-- | Get pull requests closed since a given date.
closedPullRequestsSince :: DeltaParams -> UTCTime -> UTCTime -> IO (Vector GH.SimplePullRequest)
closedPullRequestsSince DeltaParams { .. } dateSince dateUntil = do
  response <- GH.executeRequestMaybe deltaParamsAuth $
                GH.pullRequestsForR deltaParamsOwner deltaParamsRepo opts (Just 100)
  case response of
    Left err  -> error $ show err
    Right prs -> return $ V.filter hasSinceBeenMerged prs

  where
    opts :: GH.PullRequestOptions
    opts = GH.defaultPullRequestOptions
           & GH.setPullRequestOptionsState GH.PullRequestStateClosed

    hasSinceBeenMerged :: GH.SimplePullRequest -> Bool
    hasSinceBeenMerged pr =
      case GH.simplePullRequestMergedAt pr of
        Just mergedAt -> mergedAt > dateSince && mergedAt < dateUntil
        _             -> False

-- | Render internal representation as markdown.
template :: Delta -> Text
template Delta { .. } = titleTemplate <>
                        newLine <>
                        newLine <>
                        T.intercalate newLine (eventTemplate <$> deltaEvents)
  where
    eventTemplate :: Event -> Text
    eventTemplate Event { .. } = T.intercalate space
                                   ["*", eventTitle, "-", "@" <> eventAuthor, eventLink]

    newLine :: Text
    newLine = "\n"

    space :: Text
    space = " "

    titleTemplate :: Text
    titleTemplate = T.intercalate space
                      ["##", versionTemplate, deltaDateSince, "to", deltaDateUntil]

    versionTemplate :: Text
    versionTemplate = "[" <> fromMaybe "Unreleased" deltaVersion <> "]"

-- | Convert collection of pull requests to internal representation.
toDelta :: UTCTime -> UTCTime -> Vector GH.SimplePullRequest -> Maybe Text -> Delta
toDelta dateSince dateUntil prs = Delta (formatDate dateSince) (formatDate dateUntil) events
  where
    formatDate :: UTCTime -> Text
    formatDate x = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" x

    events :: [Event]
    events = V.toList $ fmap toEvent prs

-- | Convert pull request to internal representation.
toEvent :: GH.SimplePullRequest -> Event
toEvent pr = Event author title link
  where
    author :: Text
    author = GH.untagName $ GH.simpleUserLogin $ GH.simplePullRequestUser pr

    title :: Text
    title = GH.simplePullRequestTitle pr

    link :: Text
    link = GH.getUrl $ GH.pullRequestLinksHtml $ GH.simplePullRequestLinks pr
