-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Image
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Handler.Image
    ( getImageR
    ) where

import qualified Data.Text.Encoding as Text

import Yesod

import Foundation
import Model

getImageR :: Key ImagePreview -> Handler TypedContent
getImageR ident = do
    img <- runDB $ get404 ident >>= get404 . imagePreviewFullVersion
    addHeader "Content-Disposition" "inline"
    sendResponse (Text.encodeUtf8 $ imageContentType img, toContent $ imageData img)


