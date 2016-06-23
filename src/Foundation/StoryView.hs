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
    , prepareGetStories
    , prepareGetStoriesForTask
    , prepareGetStoriesForCourse
    , prepareCountStories
    , prepareCountStoriesForTask
    , prepareCountStoriesForCourse
    ) where

import qualified Data.Text as Text
import Database.Persist.Sql
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
                    . filter (not . Text.null)
                    . map (Text.dropWhile (\c -> c == ' ' || c == '\r' || c == '\t'))
                    . Text.lines
        dropInitSpace = Text.dropWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')

prepareGetStories :: ReaderT SqlBackend Handler (Int -> Int -> ReaderT SqlBackend Handler [StoryView])
prepareGetStories = do
  countryId   <- getFieldName CountryId
  countryName <- getFieldName CountryName

  placeId      <- getFieldName PlaceId
  placeCountry <- getFieldName PlaceCountry
  placeName    <- getFieldName PlaceName

  studentId <- getFieldName StudentId
  studentName <- getFieldName StudentName


  storyId           <- getFieldName StoryId
--  storyResource     <- getFieldName StoryResource
  storyAuthor       <- getFieldName StoryAuthor
  storyImage        <- getFieldName StoryImage
  storyPlace        <- getFieldName StoryPlace
  storyComment      <- getFieldName StoryComment
  storyCreationTime <- getFieldName StoryCreationTime

  oldStoryId           <- getFieldName OldStoryId
--  oldStoryResource     <- getFieldName OldStoryResource
  oldStoryImage        <- getFieldName OldStoryImage
  oldStoryComment      <- getFieldName OldStoryComment
  oldStoryTitle        <- getFieldName OldStoryTitle
  oldStoryAuthor       <- getFieldName OldStoryAuthor
  oldStoryCreationTime <- getFieldName OldStoryCreationTime

  let query = Text.unlines
          ["SELECT 1 as stype,"
          ,"       story."<>storyId<>" as sid,"
          ,"       student."<>studentName<>" as author_name,"
          ,"       place."<>placeName<>" as place,"
          ,"       country."<>countryName<>" as country,"
          ,"       null as title,"
          ,"       story."<>storyCreationTime<>" as stime,"
          ,"       story."<>storyImage<>" as simage,"
          ,"       story."<>storyComment<>" as scomment"
          ,"  FROM story"
          ,"  INNER JOIN student"
          ,"          ON story."<>storyAuthor<>" = student."<>studentId
          ,"  INNER JOIN place"
          ,"          ON place."<>placeId<>" = story."<>storyPlace
          ,"  INNER JOIN country"
          ,"          ON country."<>countryId<>" = place."<>placeCountry
          ,"UNION ALL"
          ,"SELECT 0,"
          ,"       old_story."<>oldStoryId<>","
          ,"       old_story."<>oldStoryAuthor<>","
          ,"       null,"
          ,"       null,"
          ,"       old_story."<>oldStoryTitle<>","
          ,"       old_story."<>oldStoryCreationTime<>","
          ,"       old_story."<>oldStoryImage<>","
          ,"       old_story."<>oldStoryComment<>""
          ,"  FROM old_story"
          ,"order by stime desc"
          ,"LIMIT ? OFFSET ?"
          ]
  return $ \o l -> fmap mkStoryView <$> rawSql query [toPersistValue l, toPersistValue o]

prepareGetStoriesForTask :: ReaderT SqlBackend Handler (EdxResourceId -> Int -> Int -> ReaderT SqlBackend Handler [StoryView])
prepareGetStoriesForTask = do
  countryId   <- getFieldName CountryId
  countryName <- getFieldName CountryName

  placeId      <- getFieldName PlaceId
  placeCountry <- getFieldName PlaceCountry
  placeName    <- getFieldName PlaceName

  studentId <- getFieldName StudentId
  studentName <- getFieldName StudentName


  storyId           <- getFieldName StoryId
  storyResource     <- getFieldName StoryResource
  storyAuthor       <- getFieldName StoryAuthor
  storyImage        <- getFieldName StoryImage
  storyPlace        <- getFieldName StoryPlace
  storyComment      <- getFieldName StoryComment
  storyCreationTime <- getFieldName StoryCreationTime

  oldStoryId           <- getFieldName OldStoryId
  oldStoryResource     <- getFieldName OldStoryResource
  oldStoryImage        <- getFieldName OldStoryImage
  oldStoryComment      <- getFieldName OldStoryComment
  oldStoryTitle        <- getFieldName OldStoryTitle
  oldStoryAuthor       <- getFieldName OldStoryAuthor
  oldStoryCreationTime <- getFieldName OldStoryCreationTime

  let query = Text.unlines
          ["SELECT t.stype, t.sid, t.author_name, t.place, t.country, t.title, t.stime, t.simage, t.scomment"
          ,"FROM"
          ,"(SELECT 1 as stype,"
          ,"       story."<>storyId<>" as sid,"
          ,"       story."<>storyResource<>" as sres,"
          ,"       student."<>studentName<>" as author_name,"
          ,"       place."<>placeName<>" as place,"
          ,"       country."<>countryName<>" as country,"
          ,"       null as title,"
          ,"       story."<>storyCreationTime<>" as stime,"
          ,"       story."<>storyImage<>" as simage,"
          ,"       story."<>storyComment<>" as scomment"
          ,"  FROM story"
          ,"  INNER JOIN student"
          ,"          ON story."<>storyAuthor<>" = student."<>studentId
          ,"  INNER JOIN place"
          ,"          ON place."<>placeId<>" = story."<>storyPlace
          ,"  INNER JOIN country"
          ,"          ON country."<>countryId<>" = place."<>placeCountry
          ,"UNION ALL"
          ,"SELECT 0,"
          ,"       old_story."<>oldStoryId<>","
          ,"       old_story."<>oldStoryResource<>","
          ,"       old_story."<>oldStoryAuthor<>","
          ,"       null,"
          ,"       null,"
          ,"       old_story."<>oldStoryTitle<>","
          ,"       old_story."<>oldStoryCreationTime<>","
          ,"       old_story."<>oldStoryImage<>","
          ,"       old_story."<>oldStoryComment<>""
          ,"  FROM old_story) t"
          ,"WHERE t.sres = ?"
          ,"order by t.stime desc"
          ,"LIMIT ? OFFSET ?"
          ]
  return $ \rId o l -> fmap mkStoryView <$> rawSql query [toPersistValue rId, toPersistValue l, toPersistValue o]

prepareGetStoriesForCourse :: ReaderT SqlBackend Handler (EdxCourseId -> Int -> Int -> ReaderT SqlBackend Handler [StoryView])
prepareGetStoriesForCourse = do
  edxResourceId <- getFieldName EdxResourceId
  edxResourceCourseId <- getFieldName EdxResourceCourseId

  countryId   <- getFieldName CountryId
  countryName <- getFieldName CountryName

  placeId      <- getFieldName PlaceId
  placeCountry <- getFieldName PlaceCountry
  placeName    <- getFieldName PlaceName

  studentId <- getFieldName StudentId
  studentName <- getFieldName StudentName


  storyId           <- getFieldName StoryId
  storyResource     <- getFieldName StoryResource
  storyAuthor       <- getFieldName StoryAuthor
  storyImage        <- getFieldName StoryImage
  storyPlace        <- getFieldName StoryPlace
  storyComment      <- getFieldName StoryComment
  storyCreationTime <- getFieldName StoryCreationTime

  oldStoryId           <- getFieldName OldStoryId
  oldStoryResource     <- getFieldName OldStoryResource
  oldStoryImage        <- getFieldName OldStoryImage
  oldStoryComment      <- getFieldName OldStoryComment
  oldStoryTitle        <- getFieldName OldStoryTitle
  oldStoryAuthor       <- getFieldName OldStoryAuthor
  oldStoryCreationTime <- getFieldName OldStoryCreationTime

  let query = Text.unlines
          ["SELECT t.stype, t.sid, t.author_name, t.place, t.country, t.title, t.stime, t.simage, t.scomment"
          ,"FROM"
          ,"(SELECT 1 as stype,"
          ,"       story."<>storyId<>" as sid,"
          ,"       story."<>storyResource<>" as sres,"
          ,"       student."<>studentName<>" as author_name,"
          ,"       place."<>placeName<>" as place,"
          ,"       country."<>countryName<>" as country,"
          ,"       null as title,"
          ,"       story."<>storyCreationTime<>" as stime,"
          ,"       story."<>storyImage<>" as simage,"
          ,"       story."<>storyComment<>" as scomment"
          ,"  FROM story"
          ,"  INNER JOIN student"
          ,"          ON story."<>storyAuthor<>" = student."<>studentId
          ,"  INNER JOIN place"
          ,"          ON place."<>placeId<>" = story."<>storyPlace
          ,"  INNER JOIN country"
          ,"          ON country."<>countryId<>" = place."<>placeCountry
          ,"UNION ALL"
          ,"SELECT 0,"
          ,"       old_story."<>oldStoryId<>","
          ,"       old_story."<>oldStoryResource<>","
          ,"       old_story."<>oldStoryAuthor<>","
          ,"       null,"
          ,"       null,"
          ,"       old_story."<>oldStoryTitle<>","
          ,"       old_story."<>oldStoryCreationTime<>","
          ,"       old_story."<>oldStoryImage<>","
          ,"       old_story."<>oldStoryComment<>""
          ,"  FROM old_story) t"
          ,"INNER JOIN edx_resource"
          ,"        ON edx_resource."<>edxResourceId<>" = t.sres"
          ,"WHERE edx_resource."<>edxResourceCourseId<>" = ?"
          ,"order by t.stime desc"
          ,"LIMIT ? OFFSET ?"
          ]
  return $ \rId o l -> fmap mkStoryView <$> rawSql query [toPersistValue rId, toPersistValue l, toPersistValue o]


prepareCountStoriesForCourse :: ReaderT SqlBackend Handler (EdxCourseId -> ReaderT SqlBackend Handler Int)
prepareCountStoriesForCourse = do
  edxResourceId <- getFieldName EdxResourceId
  edxResourceCourseId <- getFieldName EdxResourceCourseId
  storyResource     <- getFieldName StoryResource
  oldStoryResource     <- getFieldName OldStoryResource

  let query = Text.unlines
          ["SELECT count(t.x)"
          ,"FROM"
          ,"(SELECT story."<>storyResource<>" as x FROM story"
          ,"UNION ALL"
          ,"SELECT old_story."<>oldStoryResource<>" FROM old_story) t"
          ,"INNER JOIN edx_resource"
          ,"        ON edx_resource."<>edxResourceId<>" = t.x"
          ,"WHERE edx_resource."<>edxResourceCourseId<>" = ?"
          ]
  return $ \rId -> mkCount <$> rawSql query [toPersistValue rId]


prepareCountStoriesForTask :: ReaderT SqlBackend Handler (EdxResourceId -> ReaderT SqlBackend Handler Int)
prepareCountStoriesForTask = do
  storyResource     <- getFieldName StoryResource
  oldStoryResource     <- getFieldName OldStoryResource

  let query = Text.unlines
          ["SELECT count(t.x)"
          ,"FROM"
          ,"(SELECT story."<>storyResource<>" as x FROM story"
          ,"UNION ALL"
          ,"SELECT old_story."<>oldStoryResource<>" FROM old_story) t"
          ,"WHERE t.x = ?"
          ]
  return $ \rId -> mkCount <$> rawSql query [toPersistValue rId]


prepareCountStories :: ReaderT SqlBackend Handler (ReaderT SqlBackend Handler Int)
prepareCountStories = do
  storyResource     <- getFieldName StoryResource
  oldStoryResource     <- getFieldName OldStoryResource

  let query = Text.unlines
          ["SELECT count(t.x)"
          ,"FROM"
          ,"(SELECT story."<>storyResource<>" as x FROM story"
          ,"UNION ALL"
          ,"SELECT old_story."<>oldStoryResource<>" FROM old_story) t"
          ]
  return $ mkCount <$> rawSql query []



mkCount :: [Single Int] -> Int
mkCount [] = 0
mkCount ((Single x):_) = x

mkTitle :: Maybe Text -> Maybe Text -> Maybe Text -> Text
mkTitle (Just t) _ _ = t
mkTitle Nothing (Just c) (Just p) = c <> ", " <> p
mkTitle Nothing (Just c) Nothing = "Somewhere in " <> c
mkTitle Nothing Nothing (Just p) = p
mkTitle Nothing Nothing Nothing = "From the nowhere"

mkStoryView :: (Single Bool, Single Int64, Single (Maybe Text), (Single (Maybe Text), Single (Maybe Text)), Single (Maybe Text), Single UTCTime, Single ImagePreviewId, Single Text) -> StoryView
mkStoryView (Single True, Single sId, Single mauthor, (Single mplace, Single mcountry), Single mtitle, Single time, Single imgPId, Single comment) = StoryView
    { svRoute    = StoryR $ toSqlKey sId
    , svImage    = imgPId
    , svAuthor   = mauthor
    , svComment  = comment
    , svTime     = Text.pack . show . utctDay $ time
    , svCountry  = mcountry
    , svPlace    = mplace
    , svTitle    = mkTitle mtitle mcountry mplace
    }
mkStoryView (Single False, Single sId, Single mauthor, (Single mplace, Single mcountry), Single mtitle, Single time, Single imgPId, Single comment) = StoryView
    { svRoute    = OldStoryR $ toSqlKey sId
    , svImage    = imgPId
    , svAuthor   = mauthor
    , svComment  = comment
    , svTime     = Text.pack . show . utctDay $ time
    , svCountry  = mcountry
    , svPlace    = mplace
    , svTitle    = mkTitle mtitle mcountry mplace
    }

