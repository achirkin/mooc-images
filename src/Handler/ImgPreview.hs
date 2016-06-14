-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.ImgPreview
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

module Handler.ImgPreview
    ( getImgPreviewR
    ) where

import Import

getImgPreviewR :: Key ImagePreview -> Handler TypedContent
getImgPreviewR ident = do
    img <- runDB $ get404 ident
    addHeader "Content-Disposition" "inline"
    sendResponse (("image/png" :: ByteString), toContent $ imagePreviewContent img)


