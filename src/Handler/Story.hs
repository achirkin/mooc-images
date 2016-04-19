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
    ) where

import Data.Default
import qualified Data.Text as Text
import Data.Time
import Text.Blaze (toMarkup)

import Yesod
import Yesod.Default.Util

import Foundation
import Model

getStoryR :: Key Story -> Handler Html
getStoryR ident = do
    (story, title) <- runDB $ do
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
                                   placeName place )

    defaultLayout $ do
        setTitle $ toMarkup title
        $(widgetFileNoReload def "story")


