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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Story
    ( getStoryR
    , getOldStoryR
    ) where

import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Data.Time
import Text.Blaze (toMarkup)

import Yesod
import Yesod.Default.Util

import Foundation
import Model

getStoryR :: Key Story -> Handler Html
getStoryR ident = do
    (story, title, imgId) <- runDB $ do
        ustory <- get404 ident
        place <- get404 $ storyPlace ustory
        country <- get404 $ placeCountry place
        student <- get404 $ storyAuthor ustory
        return (( studentName student
               , storyComment ustory
               , storyCreationTime ustory
               , countryName country
               , placeName place), countryName country `Text.append`
                                   ", " `Text.append`
                                   placeName place , storyImage ustory)

    defaultLayout $ do
        setTitle $ toMarkup title
        $(widgetFileNoReload def "story")



getOldStoryR :: Key OldStory -> Handler Html
getOldStoryR ident = do
    (story, title, imgId) <- runDB $ do
        ustory <- get404 ident
        return (( oldStoryAuthor ustory
               , fromMaybe "" $ oldStoryComment ustory
               , oldStoryCreationTime ustory
               , ("no country specified" :: Text)
               , ("no place specified" :: Text) ), oldStoryTitle ustory, oldStoryImage ustory )

    defaultLayout $ do
        setTitle $ toMarkup title
        $(widgetFileNoReload def "story")
