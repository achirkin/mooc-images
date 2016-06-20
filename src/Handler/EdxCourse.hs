-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.EdxCourse
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Handler.EdxCourse
    ( getRenameCourseR
    , getRenameResourceR
    ) where

import Import


getRenameCourseR :: EdxCourseId -> Handler ()
getRenameCourseR cId = do
  mname <- lookupGetParam "newname" :: Handler (Maybe Text)
  case mname of
    Nothing -> setMessage "Cannot set empty name for a course"
    Just "" -> setMessage "Cannot set empty name for a course"
    Just name -> do
      setMessage "Successfully renamed a course."
      runDB $ update cId [EdxCourseFriendlyName =. Just name]
  _ <- redirectUltDest HomeR
  return ()

getRenameResourceR :: EdxResourceId -> Handler ()
getRenameResourceR cId = do
  mname <- lookupGetParam "newname" :: Handler (Maybe Text)
  case mname of
    Nothing -> setMessage "Cannot set empty name for a resource"
    Just "" -> setMessage "Cannot set empty name for a resource"
    Just name -> do
      setMessage "Successfully renamed a resource."
      runDB $ update cId [EdxResourceFriendlyName =. Just name]
  _ <- redirectUltDest HomeR
  return ()
