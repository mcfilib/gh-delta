{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib (
    generate,
    DeltaParams,
    defaultDeltaParams,
    setDeltaParamsAuth,
    setDeltaParamsOwner,
    setDeltaParamsRepo,
    setDeltaParamsSince,
    ) where

import           Control.Monad    (unless)
import           Data.Function    ((&))
import           Data.Monoid      ((<>))
import           Data.String      (fromString)
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import qualified GitHub           as GH

-- | Parameters required to generate a Delta.
data DeltaParams =
       DeltaParams
         { deltaAuth  :: Maybe GH.Auth
         , deltaOwner :: GH.Name GH.Owner
         , deltaRepo  :: GH.Name GH.Repo
         , deltaSince :: GH.Name GH.GitCommit
         }

-- | Default params using the gh-delta repo.
defaultDeltaParams :: DeltaParams
defaultDeltaParams =
  DeltaParams Nothing "filib" "gh-delta" "f44caa05adf066ae441cbdbebe54010d94172e9a"

-- | Setter for personal access token.
setDeltaParamsAuth :: Maybe String -> DeltaParams -> DeltaParams
setDeltaParamsAuth x params = params { deltaAuth = fmap (GH.OAuth . fromString) x }

-- | Setter for owner.
setDeltaParamsOwner :: String -> DeltaParams -> DeltaParams
setDeltaParamsOwner x params = params { deltaOwner = fromString x }

-- | Setter for repo.
setDeltaParamsRepo :: String -> DeltaParams -> DeltaParams
setDeltaParamsRepo x params = params { deltaRepo = fromString x }

-- | Setter for SHA.
setDeltaParamsSince :: String -> DeltaParams -> DeltaParams
setDeltaParamsSince x params = params { deltaSince = fromString x }

-- | Single event in a changelog.
data Event = Event { eventAuthor :: Text, eventTitle :: Text, eventLink :: Text }

-- | Single changelog entry.
data Delta = Delta { deltaDate :: Text, deltaEvents :: [Event] }

-- | Write changelog entry since SHA to STDOUT.
generate :: DeltaParams -> IO ()
generate params@DeltaParams { .. } = do
  response <- commitDate params
  case response of
    Left err -> error $ show err
    Right start -> do
      prs <- closedPullRequestsSince params start
      unless (V.null prs) $
        T.putStrLn $ template (toDelta start prs)

-- | Get date a commit was created.
commitDate :: DeltaParams -> IO (Either GH.Error UTCTime)
commitDate DeltaParams { .. } = do
  response <- GH.executeRequestMaybe deltaAuth $ GH.gitCommitR deltaOwner deltaRepo deltaSince
  case response of
    Left err     -> return $ Left err
    Right commit -> return $ Right (GH.gitUserDate $ GH.gitCommitAuthor commit)

-- | Get pull requests closed since a given date.
closedPullRequestsSince :: DeltaParams -> UTCTime -> IO (Vector GH.SimplePullRequest)
closedPullRequestsSince DeltaParams { .. } since = do
  response <- GH.executeRequestMaybe deltaAuth $
                 GH.pullRequestsForR deltaOwner deltaRepo opts (Just 100)
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
        Just mergedAt -> since < mergedAt
        _             -> False

-- | Render internal representation as markdown.
template :: Delta -> Text
template Delta { .. } = T.intercalate " "
                          [ "##"
                          , deltaDate
                          , "\n"
                          , T.intercalate " " $ fmap eventTemplate deltaEvents
                          ]
  where
    eventTemplate :: Event -> Text
    eventTemplate Event { .. } = T.intercalate " " [eventTitle, "-", "@" <> eventAuthor, eventLink]

-- | Convert collection of pull requests to internal representation.
toDelta :: UTCTime -> Vector GH.SimplePullRequest -> Delta
toDelta start prs = Delta date events
  where
    date :: Text
    date = T.pack $ formatTime defaultTimeLocale "%d %b %Y" start

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
