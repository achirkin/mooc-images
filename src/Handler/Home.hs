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
import Data.Default
import Data.Time
import Data.Text (Text)
--import Database.Persist.Sql (fromSqlKey)
import qualified Data.Text as Text
--import qualified Data.ByteString as S
--import qualified Data.ByteString.Lazy as L
import Yesod
import Yesod.Default.Util
--import Yesod.Auth

import Foundation
import Model

getHomeR :: Handler Html
getHomeR = do
    storedFiles <- getImages >>= mapM (\(Entity i story) -> runDB $ do
        place <- get404 $ storyPlace story
        country <- get404 $ placeCountry place
        student <- get404 $ storyAuthor story
        return (i, studentName student
               , storyComment story
               , storyCreationTime story
               , countryName country
               , placeName place)
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


getImages :: Handler [Entity Story]
getImages = runDB $ selectList [] [Desc StoryCreationTime]
