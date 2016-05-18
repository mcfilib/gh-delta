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
    setDeltaParamsLabel,
    ) where

import           Data.Function    ((&))
import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import           Data.String      (fromString)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time.Clock  (UTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import qualified GitHub           as GH

-- | Parameters required to generate a Delta.
data DeltaParams =
       DeltaParams
         { deltaParamsAuth  :: Maybe GH.Auth
         , deltaParamsOwner :: GH.Name GH.Owner
         , deltaParamsRepo  :: GH.Name GH.Repo
         , deltaParamsSince :: GH.Name GH.GitCommit
         , deltaParamsUntil :: Maybe (GH.Name GH.GitCommit)
         , deltaParamsLabel :: Maybe Text
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

-- | Setter for label.
setDeltaParamsLabel :: Maybe String -> DeltaParams -> DeltaParams
setDeltaParamsLabel x params = params { deltaParamsLabel = fmap fromString x }

-- | Single event in a changelog.
data Event = Event { eventAuthor :: Text, eventTitle :: Text, eventLink :: Text }

-- | Single changelog entry.
data Delta =
       Delta
         { deltaDateSince :: Text
         , deltaDateUntil :: Text
         , deltaEvents    :: [Event]
         , deltaLabel     :: Maybe Text
         }

-- | Generate changelog or produce a meaningful error.
generate :: DeltaParams -> IO (Either String Text)
generate params@DeltaParams { .. } = do
  dateSinceResponse <- commitDate params deltaParamsSince
  case dateSinceResponse of
    Left err -> failure $ renderError err
    Right dateSince ->
      case deltaParamsUntil of
        Just sha -> do
          dateUntilResponse <- commitDate params sha
          case dateUntilResponse of
            Left err -> failure $ renderError err
            Right dateUntil -> do
              pullRequests <- fetchPullRequests dateSince dateUntil
              success $ renderTemplate dateSince dateUntil pullRequests
        Nothing -> do
          dateUntil <- getCurrentTime
          pullRequests <- fetchPullRequests dateSince dateUntil
          success $ renderTemplate dateSince dateUntil pullRequests

  where
    fetchPullRequests :: UTCTime -> UTCTime -> IO (Vector GH.SimplePullRequest)
    fetchPullRequests = closedPullRequestsSince params

    renderError :: (Show a) => a -> String
    renderError = show

    renderTemplate :: UTCTime -> UTCTime -> Vector GH.SimplePullRequest -> Text
    renderTemplate x y z = template (toDelta x y z deltaParamsLabel)

    failure :: (Monad m) => a -> m (Either a b)
    failure = return . Left

    success :: (Monad m) => b -> m (Either a b)
    success = return . Right

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
    titleTemplate = T.intercalate space ["##", labelTemplate, deltaDateSince, "to", deltaDateUntil]

    labelTemplate :: Text
    labelTemplate = "[" <> fromMaybe "Unreleased" deltaLabel <> "]"

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
