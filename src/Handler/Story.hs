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
    story <- runDB $ get404 ident >>= viewStory . Entity ident
    let deleteStory = DeleteStoryR ident
    defaultLayout $ do
        setTitle $ toMarkup $ svTitle story
        $(widgetFile "story")



getOldStoryR :: Key OldStory -> Handler Html
getOldStoryR ident = do
    muser <- fmap entityVal <$> maybeAuth
    story <- runDB $ get404 ident >>= viewOldStory . Entity ident
    let deleteStory = DeleteOldStoryR ident
    defaultLayout $ do
        setTitle $ toMarkup $ svTitle story
        $(widgetFile "story")

getDeleteStoryR :: Key Story -> Handler ()
getDeleteStoryR ident = do
  muser <- fmap entityVal <$> maybeAuth
  when (isAdmin muser) . runDB $ delete ident

getDeleteOldStoryR :: Key OldStory -> Handler ()
getDeleteOldStoryR ident = do
  muser <- fmap entityVal <$> maybeAuth
  when (isAdmin muser) . runDB $ delete ident
