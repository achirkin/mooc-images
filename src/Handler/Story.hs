-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Story
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Handler.Story
    ( getStoryR
    , getOldStoryR
    , getDeleteStoryR
    , getDeleteOldStoryR
    ) where

import Text.Blaze (toMarkup)
import Import

getStoryR :: Key Story -> Handler Html
getStoryR ident = do
    muser <- fmap entityVal <$> maybeAuth
    (story, resId) <- runDB $ get404 ident >>= \s -> do
       vs <- viewStory $ Entity ident s
       return (vs, storyResource s )
    storyLocation <- makeLocation resId
    let deleteStory = DeleteStoryR ident
    defaultLayout $ do
        setTitle $ toMarkup $ svTitle story
        $(widgetFile "story")



getOldStoryR :: Key OldStory -> Handler Html
getOldStoryR ident = do
    muser <- fmap entityVal <$> maybeAuth
    (story, resId) <- runDB $ get404 ident >>= \s -> do
       vs <- viewOldStory $ Entity ident s
       return (vs, oldStoryResource s )
    storyLocation <- makeLocation resId
    let deleteStory = DeleteOldStoryR ident
    defaultLayout $ do
        setTitle $ toMarkup $ svTitle story
        $(widgetFile "story")

getDeleteStoryR :: Key Story -> Handler ()
getDeleteStoryR ident = runDB $ delete ident

getDeleteOldStoryR :: Key OldStory -> Handler ()
getDeleteOldStoryR ident = runDB $ delete ident


makeLocation :: EdxResourceId -> Handler Text
makeLocation resId = runDB $ do
  mres <- get resId
  case mres of
    Nothing  -> return "Unknown exercise"
    Just res -> do
      mcourse <- get $ edxResourceCourseId res
      return $ case (getCourseName mcourse, getResName mres) of
        ("", "") -> "Unknown exercise"
        ("", s)  -> s
        (s, "")  -> s
        (s,t)    -> s <> " - " <> t
  where
    getCourseName mcourse = case mcourse of
      Nothing -> ""
      Just course -> fromMaybe (edxCourseContextId course) (edxCourseFriendlyName course)
    getResName mres = case mres of
      Nothing -> ""
      Just res -> fromMaybe (edxResourceLink res) (edxResourceFriendlyName res)
