-----------------------------------------------------------------------------
-- |
-- Module      :  Foundation.StoryView
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Foundation.StoryView
    ( StoryView (..)
    , viewStory, viewOldStory, shortComment
    ) where

import qualified Data.Text as Text
import Import.NoFoundation
import Foundation

-- | A simple to view data type that unifies Story and OldStory
data StoryView = StoryView
  { svRoute    :: Route App
  , svImage    :: ImagePreviewId
  , svAuthor   :: Maybe Text
  , svComment  :: Text
  , svTime     :: Text
  , svCountry  :: Maybe Text
  , svPlace    :: Maybe Text
  , svTitle    :: Text
  }


viewStory :: Entity Story -> ReaderT SqlBackend Handler StoryView
viewStory (Entity i story) = do
  place <- get404 $ storyPlace story
  country <- get404 $ placeCountry place
  student <- get404 $ storyAuthor story
  return $ StoryView
    { svRoute    = StoryR i
    , svImage    = storyImage story
    , svAuthor   = studentName student
    , svComment  = storyComment story
    , svTime     = Text.pack . show . utctDay $ storyCreationTime story
    , svCountry  = Just $ countryName country
    , svPlace    = Just $ placeName place
    , svTitle    = countryName country <> ", " <> placeName place
    }


viewOldStory :: Entity OldStory -> ReaderT SqlBackend Handler StoryView
viewOldStory (Entity i story) = return StoryView
    { svRoute    = OldStoryR i
    , svImage    = oldStoryImage story
    , svAuthor   = oldStoryAuthor story
    , svComment  = fromMaybe "" $ oldStoryComment story
    , svTime     = Text.pack . show . utctDay $ oldStoryCreationTime story
    , svCountry  = Nothing
    , svPlace    = Nothing
    , svTitle    = oldStoryTitle story
    }

shortLength :: Int
shortLength = 140

maxLines :: Int
maxLines = 3

shortComment :: StoryView -> Text
shortComment sv = dropInitSpace . remNewLines $
  if Text.length t < shortLength
    then t
    else remLong t <> "..."
  where t = svComment sv
        remLong = Text.dropEnd 1
                . Text.dropWhileEnd (\c -> c /= ' ' && c /= '\n' && c /= '\t')
                . Text.take shortLength
        remNewLines = Text.dropWhileEnd (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
                    . Text.unlines
                    . take maxLines
                    . Text.lines
        dropInitSpace = Text.dropWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
