{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Application
import qualified Data.Binary.Builder   as BB
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Char             as Char
import qualified Data.Conduit.Binary   as CB
import qualified Data.Text             as Text
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Data.Yaml             as Yaml
import           Database.Persist.Sql  (fromSqlKey)
import           Import
import           Network.Wai
import           System.Directory
import           System.FilePath
import           System.Log.FastLogger

import           Handler.Home
import           Handler.Story

getOutDir :: IO FilePath
getOutDir = do
  curDateStr <- showGregorian . utctDay <$> getCurrentTime
  return $ "mooc-images-" <> curDateStr


saveHandlerContent :: ToContent a => App -> FilePath -> Handler a -> IO ()
saveHandlerContent app outPath h = unsafeHandler app (h >>= saveContent outPath)


saveContent :: (ToContent a, MonadIO m) => FilePath -> a -> m ()
saveContent outPath x = liftIO $ do
  createCheckDir outPath
  bs <- contentToBs $ toContent x
  BS.writeFile outPath bs

contentToBs :: Content -> IO ByteString
contentToBs (ContentDontEvaluate content)
  = contentToBs content -- I try :)
contentToBs (ContentFile p Nothing)
  = readFile p
contentToBs (ContentFile p (Just part))
  = BS.take (fromInteger $ filePartByteCount part)
  . BS.drop (fromInteger $ filePartOffset part)
  <$> readFile p
contentToBs (ContentBuilder b Nothing)
  = return . BSL.toStrict $ BB.toLazyByteString b
contentToBs (ContentBuilder b (Just n))
  = return . BSL.toStrict . BSL.take (fromIntegral n) $ BB.toLazyByteString b
contentToBs (ContentSource src)
  = fmap BSL.toStrict $ runResourceT $ runConduit
  $ src .| awaitForever (yield . f) .| CB.sinkLbs
  where
    f Flush     = mempty
    f (Chunk b) = BSL.toStrict $ BB.toLazyByteString b

createCheckDir :: FilePath -> IO ()
createCheckDir p = createDirectoryIfMissing True (takeDirectory p)

makeApp :: IO App
makeApp = do
    appSettings' <- getAppSettings
    let appSettings = appSettings' { appRoot = Just "." }
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <- static (appStaticDir appSettings)
    appConnPool <- createPoolConfig $ appDatabaseConf appSettings
    return App {..}


main :: IO ()
main = do
  app <- makeApp
  od <- getOutDir

  storyFolders <- unsafeHandler app $ getStoryFolders od
  unsafeHandler app $ writeIndexHtml od storyFolders
  forM_ storyFolders $ \sf -> do
    unsafeHandler app $ saveStoryFolder 100 sf
    print ( folderDir sf
          , length $ folderOStoryKeys sf
          , length $ folderStoryKeys sf
          )


writeIndexHtml :: FilePath -> [StoryFolder] -> Handler ()
writeIndexHtml od folders = do
    app <- getYesod
    let baseDir = rootDir fileLoc
        routeUrl route = yesodRender app (Text.pack baseDir) (StaticR route) []
        fileLoc = od </> "index.html"
        sfIndex p = baseDir </> folderDir p </> "index.html"
    content <- defaultLayout $ do
      setTitle "EdX User Stories"
      toWidgetHead [hamlet|
        <style>
          .hoverbutton{padding:0 16px 24px 16px;top:0px;left:0px;position:absolute;width:100%;height:100%}.hoverbutton a{width:100%;height:100%}.card .card-side-moocimg{max-width:50%;background-color:#FFFFFF}.card-inner .card-heading{line-height:inherit;margin-top:auto;margin-bottom:auto}.headingline{margin:0;padding:0;display:block;float:left}.comment-preview{white-space:pre-line;overflow-y:hidden;color:#555}.modal-title-icon{color:#727272;cursor:pointer;display:block;font-size:20px}.modal-title-icon:focus,.modal-title-icon:hover{text-decoration:none}.container-hovered-show{opacity:0.01;filter:alpha(opacity=1)}li:hover > .container-hovered-show{opacity:1.0;filter:alpha(opacity=100)}body{font-size:10pt}
      |]
      [whamlet|
        <!-- css -->
        <link href="#{routeUrl css_base_min_css}" rel="stylesheet">
        <link href="#{routeUrl css_project_min_css}" rel="stylesheet">

        <!-- header of the page -->
        <header class="header header-transparent header-waterfall ui-header affix-top">
          <ul class="nav nav-list pull-left" style="padding: 16px;">
            <li>
              <span class="icon icon-lg">menu
          <div.header-logo.margin-no style="word-break: break-all; word-wrap: break-word; width: 70%; display: inline-block; padding-top: 12px;">
            Browse exercises

        <!-- content -->
        <main class="content">
          <div class="content-header ui-content-header">
            <div class="container">
              <h1 class="content-heading">EdX User Stories
          <div class="container">
            <section class="content-inner margin-top-no">

                <!-- all story cards -->
                <div class="ui-card-wrap">
                  <ul class="nav">
                    $forall folder <- folders
                      $case (folderCourse folder, folderResource folder)
                        $of (Entity _ course, Entity _ resource)
                          <li>
                            <a class="waves-attach waves-effect" href="#{sfIndex folder}">
                              $maybe cname <- edxCourseFriendlyName course
                                #{cname}
                              $nothing
                                #{edxCourseContextId course}
                              -
                              $maybe rname <- edxResourceFriendlyName resource
                                #{rname}
                              $nothing
                                #{edxResourceLink resource}


        <script type="text/javascript" src="#{routeUrl js_jquery_2_2_3_min_js}">
        <script type="text/javascript" src="#{routeUrl js_base_min_js}">
        <script type="text/javascript" src="#{routeUrl js_project_min_js}">
      |]
    saveContent fileLoc content

getStoryFolders :: FilePath -> Handler [StoryFolder]
getStoryFolders od = fmap (filter hasStories) . runDB $ do
  courses <- getCourses
  fmap join $ forM courses $ \ec@(Entity coId course) -> do
    let courseDir = od </> Text.unpack (courseNiceName course)
    resources <- getResources coId
    forM resources $ \er@(Entity resId resource) -> do
      let resourceDir = courseDir </> Text.unpack (resourceNiceName resource)
      oldStoryKeys <- selectKeysList [OldStoryResource ==. resId] [Asc OldStoryCreationTime]
      storykeys <- selectKeysList [StoryResource ==. resId] [Asc StoryCreationTime]
      return StoryFolder
        { folderCourse     = ec
        , folderResource   = er
        , folderDir        = resourceDir
        , folderOStoryKeys = oldStoryKeys
        , folderStoryKeys  = storykeys
        }
  where
    hasStories sf = length (folderOStoryKeys sf) + length (folderStoryKeys sf) > 0

data StoryFolder = StoryFolder
  { folderCourse     :: Entity EdxCourse
  , folderResource   :: Entity EdxResource
  , folderDir        :: FilePath
  , folderOStoryKeys :: [Key OldStory]
  , folderStoryKeys  :: [Key Story]
  }



saveStoryFolder :: Int -- ^ max items in a folder
                -> StoryFolder
                -> Handler ()
saveStoryFolder maxItems sf@StoryFolder {..} = do
    liftIO $ createDirectoryIfMissing True folderDir
    oldStories <- runDB $ forM folderOStoryKeys $ \k ->
      fmap ((,) ("o" <> show (fromSqlKey k))) $ get404 k >>= viewOldStory . Entity k
    newStories <- runDB $ forM folderStoryKeys $ \k ->
      fmap ((,) ("n" <> show (fromSqlKey k))) $ get404 k >>= viewStory . Entity k
    let stories :: [[(String, StoryView)]]
        stories = chunkList maxItems $ oldStories ++ newStories
        chunkList _ [] = []
        chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs
    pathsStories <- case stories of
      []   -> return []
      [xs] -> mapM (\(d, s) -> saveS (folderDir </> d) s) xs
      xss  -> fmap join $ mapM (\(i, xs) ->
                         mapM (\(d, s) ->
                                saveS (folderDir </> show i </> d) s) xs
                    ) $ zip [(1 :: Int)..] xss
    writeSFHtml sf pathsStories
  where
    saveS d s = (d, s) <$ saveStory d (entityKey folderResource) s

writeSFHtml :: StoryFolder -> [(FilePath, StoryView)] -> Handler ()
writeSFHtml StoryFolder {..} stories = do
    app <- getYesod
    let baseDir = rootDir fileLoc
        routeUrl route = yesodRender app (Text.pack baseDir) (StaticR route) []
        fileLoc = folderDir </> "index.html"
        svPreview p = baseDir </> p </> "preview.png"
        svDetails p = baseDir </> p </> "story.html"
    currentFolder <- makeLocation $ entityKey folderResource
    content <- defaultLayout $ do
      setTitle $ toHtml currentFolder
      toWidgetHead [hamlet|
        <style>
          .hoverbutton{padding:0 16px 24px 16px;top:0px;left:0px;position:absolute;width:100%;height:100%}.hoverbutton a{width:100%;height:100%}.card .card-side-moocimg{max-width:50%;background-color:#FFFFFF}.card-inner .card-heading{line-height:inherit;margin-top:auto;margin-bottom:auto}.headingline{margin:0;padding:0;display:block;float:left}.comment-preview{white-space:pre-line;overflow-y:hidden;color:#555}.modal-title-icon{color:#727272;cursor:pointer;display:block;font-size:20px}.modal-title-icon:focus,.modal-title-icon:hover{text-decoration:none}.container-hovered-show{opacity:0.01;filter:alpha(opacity=1)}li:hover > .container-hovered-show{opacity:1.0;filter:alpha(opacity=100)}body{font-size:10pt}
      |]
      [whamlet|
        <!-- css -->
        <link href="#{routeUrl css_base_min_css}" rel="stylesheet">
        <link href="#{routeUrl css_project_min_css}" rel="stylesheet">

        <!-- header of the page -->
        <header class="header header-transparent header-waterfall ui-header affix-top">
          <ul class="nav nav-list pull-left" style="padding: 16px;">
            <li>
              <span class="icon icon-lg">menu
          <div.header-logo.margin-no style="word-break: break-all; word-wrap: break-word; width: 70%; display: inline-block; padding-top: 12px;">
            #{currentFolder}

        <!-- content -->
        <main class="content">
          <div class="content-header ui-content-header">
            <div class="container">
              <h1 class="content-heading">EdX User Stories
          <div class="container">
            <section class="content-inner margin-top-no">

              <!-- all story cards -->
              <div class="ui-card-wrap">
                <div class="row">
                  $if null stories
                  $else
                    $forall (stPath, f) <- stories
                      <div class="col-lg-4 col-md-6 col-sm-9 col-xs-9 story_cards">
                        <div class="card">
                          <aside class="card-side card-side-img pull-left card-side-moocimg">
                            <img src="#{svPreview stPath}" width="200px" height="200px">
                          <div class="card-main">
                            <div class="card-inner" style="margin: 10px 12px;">
                              <p class="card-heading" style="margin: 6px 0px;">
                                <span style="display: inline-block">#{svTitle f}
                              <p style="margin: 6px 0px;">
                                $case svAuthor f
                                  $of Nothing
                                  $of Just sname
                                    #{sname}<br>
                                #{svTime f}
                              <p class="comment-preview" style="margin: 6px 0px;">
                                #{shortComment f}
                        <div class="hoverbutton">
                          <a class="btn btn-flat waves-attach call-modal" href="#{svDetails stPath}">

        <script type="text/javascript" src="#{routeUrl js_jquery_2_2_3_min_js}">
        <script type="text/javascript" src="#{routeUrl js_base_min_js}">
        <script type="text/javascript" src="#{routeUrl js_project_min_js}">
      |]
    saveContent fileLoc content


saveStory :: FilePath -- ^ folder
          -> Key EdxResource
          -> StoryView
          -> Handler ()
saveStory dir resId  sv@StoryView {..} = do
    liftIO $ createDirectoryIfMissing True dir
    writeImgPreview dir svImage
    writeImage dir svImage
    writeStoryHtml dir resId sv
    liftIO $ Yaml.encodeFile (dir </> "description.yaml") $ Yaml.object
      [ "title"   .= svTitle
      , "author"  .= fromMaybe "anonymous" svAuthor
      , "time"    .= svTime
      , "country" .= fromMaybe "unknown" svCountry
      , "place"   .= fromMaybe "unknown" svPlace
      , "comment" .= svComment
      ]


writeStoryHtml :: FilePath -> Key EdxResource -> StoryView -> Handler ()
writeStoryHtml dir resId story = do
    app <- getYesod
    let routeUrl route = yesodRender app (Text.pack $ rootDir fileLoc) (StaticR route) []
    storyLocation <- makeLocation resId
    imgname <- ("image" <.>) <$> imageExt (svImage story)
    content <- defaultLayout $ do
      setTitle $ toHtml $ svTitle story
      [whamlet|
        <link href="#{routeUrl css_base_min_css}" rel="stylesheet">
        <link href="#{routeUrl css_project_min_css}" rel="stylesheet">

        <div class="modal-dialog">
          <div class="modal-content">
            <div class="modal-heading">
              <p class="modal-title">
                #{svTitle story}
              <p style="white-space: pre-line">
                #{storyLocation}
            <div style="margin: 0 10px 0 10px; text-align: center">
              <img src="#{imgname}" style="max-width: 100%; max-height: 60vh">
            <div class="modal-inner">
              <p style="font-style: italic">
                Uploaded on #{svTime story}
                $case svAuthor story
                  $of Nothing
                  $of Just sname
                    \ by #{sname}
              <p style="white-space: pre-line">
                #{svComment story}
      |]
    saveContent fileLoc content
  where
    fileLoc = dir </> "story.html"


-- | get the path to the root dir relative to this file (not a folder!)
rootDir :: FilePath -> FilePath
rootDir f = join
        $ replicate (length (splitPath f) - 1) (".." ++ [pathSeparator])



writeImgPreview :: FilePath -> Key ImagePreview -> Handler ()
writeImgPreview dir ident = do
    img <- runDB $ get404 ident
    saveContent (dir </> "preview.png") $ imagePreviewContent img

writeImage :: FilePath -> Key ImagePreview -> Handler ()
writeImage dir ident = do
    img <- runDB $ get404 ident >>= get404 . imagePreviewFullVersion
    let ext = contentTypeToExt $ imageContentType img
    saveContent (dir </> "image" <.> Text.unpack ext) $ imageData img

imageExt :: Key ImagePreview -> Handler String
imageExt ident = do
    img <- runDB $ get404 ident >>= get404 . imagePreviewFullVersion
    return $ Text.unpack . contentTypeToExt $ imageContentType img


courseNiceName :: EdxCourse -> Text
courseNiceName c = case encodeNice <$> edxCourseFriendlyName c of
  Nothing -> encodeNice $ edxCourseContextId c
  Just "" -> encodeNice $ edxCourseContextId c
  Just fn -> fn

resourceNiceName :: EdxResource -> Text
resourceNiceName r = case encodeNice <$> edxResourceFriendlyName r of
  Nothing -> encodeNice $ edxResourceLink r
  Just "" -> encodeNice $ edxResourceLink r
  Just fn -> fn



contentTypeToExt :: Text -> Text
contentTypeToExt "image/png"
  = ".png"
contentTypeToExt "image/tiff"
  = ".tiff"
contentTypeToExt "image/bmp"
  = ".bmp"
contentTypeToExt "image/jpeg"
  = ".jpg"
contentTypeToExt "image/gif"
  = ".gif"
contentTypeToExt "image/svg+xml"
  = ".svg"
contentTypeToExt "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  = ".xlsx"
contentTypeToExt "application/msword"
  = ".doc"
contentTypeToExt "text/html"
  = ".html"
contentTypeToExt "application/pdf"
  = ".pdf"
contentTypeToExt "application/octet-stream"
  = ".bin"
contentTypeToExt "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  = ".pptx"
contentTypeToExt "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  = ".docx"
contentTypeToExt "application/vnd.oasis.opendocument.text"
  = ".odt"
contentTypeToExt "application/vnd.openxmlformats"
  = ".xml"
contentTypeToExt _
  = ".bin"



encodeNice :: Text -> Text
encodeNice
   = Text.pack . g . Text.unpack
   . Text.dropWhileEnd (not . Char.isAlphaNum)
   . Text.dropWhile (not . Char.isAlphaNum)
   . Text.map f
  where
    f '-' = '-'
    f '+' = '-'
    f ':' = '-'
    f c
      | not (Char.isAscii c) = '_'
      | Char.isAlphaNum c = Char.toLower c
      | otherwise = '_'
    g []           = []
    g [c]          = [c]
    g ('_':'_':cs) = g ('_':cs)
    g ('_':'-':cs) = g ('-':cs)
    g ('-':'-':cs) = g ('-':cs)
    g ('-':'_':cs) = g ('-':cs)
    g (a:b:cs)     = a : g (b:cs)
