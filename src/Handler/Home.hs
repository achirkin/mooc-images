{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Home where


--import Control.Monad.Trans.Resource (runResourceT)
--import Data.Conduit
--import Data.Conduit.Binary
import Database.Persist.Sql
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
--import qualified Data.ByteString as S
--import qualified Data.ByteString.Lazy as L
import Yesod
import Yesod.Default.Util
--import Yesod.Auth

import Foundation
import Model

getHomeR :: Handler Html
getHomeR = do
    l <- maybeLimit <$> lookupGetParam "l"
    o' <- maybeOffset <$> lookupGetParam "o"
    totalNumber <- runDB $ count ([] :: [Filter Story])
    let p = o' `div` l
        o = p * l
        pages = zip ([0..]:: [Int]) [0,l..totalNumber]
        proute = HomeR
    storedFiles <- getStories o l >>= mapM (\(Entity i story) -> runDB $ do
        place <- get404 $ storyPlace story
        country <- get404 $ placeCountry place
        student <- get404 $ storyAuthor story
        return (ImgPreviewR $ storyImage story, studentName student
               , storyComment story
               , storyCreationTime story
               , countryName country
               , placeName place
               , StoryR i
               , (Nothing :: Maybe Text))
      )

    defaultLayout $ do
        setTitle "EdX User Stories"
        $(widgetFileNoReload def "home")

shortLength :: Int
shortLength = 140

maxLines :: Int
maxLines = 3

shortenText :: Text -> Text
shortenText t = dropInitSpace . remNewLines $
  if Text.length t < shortLength
    then t
    else remLong t `Text.append` "..."
  where remLong = Text.dropEnd 1
                . Text.dropWhileEnd (\c -> c /= ' ' && c /= '\n' && c /= '\t')
                . Text.take shortLength
        remNewLines = Text.dropWhileEnd (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
                    . Text.unlines
                    . take maxLines
                    . Text.lines
        dropInitSpace = Text.dropWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')


getStories :: Int -> Int -> Handler [Entity Story]
getStories offset limit = runDB $ selectList [] [Desc StoryCreationTime, OffsetBy offset, LimitTo limit]

getOldStories :: Int -> Int -> Handler [Entity OldStory]
getOldStories offset limit = runDB $ selectList [] [Desc OldStoryCreationTime, OffsetBy offset, LimitTo limit]


getOldHomeR :: Handler Html
getOldHomeR = do
    l <- maybeLimit <$> lookupGetParam "l"
    o' <- maybeOffset <$> lookupGetParam "o"
    totalNumber <- runDB $ count ([] :: [Filter OldStory])
    let p = o' `div` l
        o = p * l
        pages = zip ([0..]:: [Int]) [0,l..totalNumber]
        proute = OldHomeR
    storedFiles <- getOldStories o l >>= mapM (\(Entity i story) -> runDB $ do
        return (ImgPreviewR $ oldStoryImage story, oldStoryAuthor story
               , fromMaybe "" $ oldStoryComment story
               , oldStoryCreationTime story
               , ("no country specified" :: Text)
               , ("no place specified" :: Text)
               , OldStoryR i
               , Just $ oldStoryTitle story)
      )

    defaultLayout $ do
        setTitle "EdX User Stories"
        $(widgetFileNoReload def "home")



-- | Default number of results to return
ndef :: Int
ndef = 200

-- | Maximum number of results to return
nmax :: Int
nmax = 1000

maybeOffset :: Maybe Text -> Int
maybeOffset Nothing = 0
maybeOffset (Just m) = case Text.decimal m of
    Left _ -> 0
    Right (n,_) -> n

maybeLimit :: Maybe Text -> Int
maybeLimit Nothing = ndef
maybeLimit (Just m) = case Text.decimal m of
    Left _ -> ndef
    Right (n,_) -> min nmax n

