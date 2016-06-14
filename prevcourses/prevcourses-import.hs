-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- The executable is intedent for importing old images from edX discussions,
-- sorted into folders.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Main
    ( main
    ) where


import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import Database.Persist.Sql
--import Database.Persist
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as Text
--import System.IO
import System.Environment
import System.Directory
import qualified System.EasyFile as Path
import Control.Monad
import Data.Monoid ((<>))
import Data.List (isPrefixOf, isSuffixOf)
import Data.Time.Format
import Graphics.GD.ByteString

import Config
import Model
import Control.Monad.Trans.Maybe
import Control.Exception (SomeException,try)
import Data.Maybe (catMaybes)

type DBEnv x = ReaderT (PersistEntityBackend x) IO


-- | size of preview image in pixels
previewSize :: Int
previewSize = 400

main :: IO ()
main = do
    fname:_ <- getArgs
    pool <- createPoolConfig persistConfig
    flip runSqlPool pool $ do
      -- create tables
      runMigration migrateAll
      -- import course ids
      courses <- setCourses fname
      -- import exercies ids
      forM_ courses $  \(cFolder, course) -> do
        exercises <- setExercises cFolder course
        stats <- forM exercises $ \(eFolder, exId) -> do
          comms <- liftIO $ listDirectory eFolder >>= filterM doesDirectoryExist . map (Path.combine eFolder)
          fmap ((,) eFolder) . forM comms $ \cPath -> do
            ids <- persistStory exId cPath
            return $ length ids
        liftIO $ do
          putStrLn cFolder
          mapM_ print stats
--        liftIO $ do
--          putStrLn . Text.unpack $ "course " <> Text.pack (show course) <> " at" <> Text.pack cFolder
--          forM_ exercises $ putStrLn . ("\t" ++) . show . snd
--
--          let (ex1p, ex1Id) =  head exercises
--          comms <- listDirectory ex1p >>= filterM (doesDirectoryExist . Path.combine ex1p)
--          let comm1p = Path.combine ex1p $ head comms
--          hasD <- elem "description.txt" <$> listDirectory comm1p
--          print hasD
--          when hasD $ do
--            mos <- parseDiscussionTxt (Path.combine comm1p "description.txt")
--            case mos of
--              Nothing -> return ()
--              Just f -> print $ f ex1Id undefined
    putStrLn "Finished import of previous courses"



-- I rely on a structure of description provided by chairia-internal-tools mooc-images downloading app
parseDiscussionTxt :: FilePath -> IO (Maybe (EdxResourceId -> ImagePreviewId -> OldStory))
parseDiscussionTxt file = runResourceT $
    CB.sourceFile file =$= CT.decodeUtf8Lenient =$= CT.lines $$ parseLines
  where
    parseLine t s = let (key', val') = Text.breakOn "=" s
                    in if Text.strip key' == t then return $ Text.strip . Text.drop 1 $ val'
                                               else mzero
    parseLines = runMaybeT $ do
      --Discussion:
      _ <- MaybeT await
      --  discussionId = 45c4090cec1...51d14189aa
      _ <- MaybeT await
      --  commentId    = 561df0.....0005d6
      _ <- MaybeT await
      --  authorId     = 27...2
      _ <- MaybeT await
      --  authorName   = Tibiti
      authorName <- MaybeT await >>= parseLine "authorName"
      --  title        = Climate of Pécs
      title <- MaybeT await >>= parseLine "title"
      --  courseId     = course-v1:ETHx+FC-02x+2T2015
      _ <- MaybeT await
      --  dateTime     = 2015-10-14T06:04:08.074Z
      dateTime <- MaybeT await >>= parseLine "dateTime"  >>= parseTimeM
                              True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
                              . Text.unpack . fst . Text.breakOn "."
      _ <- MaybeT await
      --  comment      =
      _ <- MaybeT await
      -- whole comment text
      comment <- Text.unlines . map Text.strip <$> lift CL.consume
      return $ \rId imgId -> OldStory
        { oldStoryResource = rId
        , oldStoryImage = imgId
        , oldStoryComment = Just comment
        , oldStoryTitle = title
        , oldStoryAuthor = Just authorName
        , oldStoryCreationTime = dateTime
        }

--Discussion:
--  discussionId = 45c4090cec1573731b50519155a63151d14189aa
--  commentId    = 561df058d2aca5fa7b0005d6
--  authorId     = 2739132
--  authorName   = Tibiti
--  title        = Climate of Pécs
--  courseId     = course-v1:ETHx+FC-02x+2T2015
--  dateTime     = 2015-10-14T06:04:08.074Z
--  comment      =
--    I live in pleasant climate in Eastern Europe in Hungary. We like to say that the climate here is sub-Mediterranean, but the fact is, that the climate is continental. The city lies under a mountain on the south side. So it is really a sunny place fanced from the wind:) In the downtown there are no green surfaces, and we have no river or any lake. ![Downtown of Pécs without green surfaces][1] Fortunately, there are no polluting industries in the city. Instead of rivers we have fountains, instead of green surfaces we have oleanders in huge pots and instead of foliage we have textile screens between the houses.![Oleanders and textile screens][2]
--    
--    Our home is cc.7 km from the downtown, in a valley. The temperature in the summer is always lower with 4-5 C than in the middle of the city.![Our garden, 7 km from downtown][3]
--    
--    
--      [1]: https://edxuploads.s3.amazonaws.com/14448018757955629.jpg
--      [2]: https://edxuploads.s3.amazonaws.com/14448025174794569.jpg
--      [3]: https://edxuploads.s3.amazonaws.com/14448020215121033.jpg


setCourses :: FilePath -> DBEnv EdxCourse [(FilePath, EdxCourseId)]
setCourses fname = do
  courseNames <- liftIO $ listDirectory fname >>= filterM (doesDirectoryExist . Path.combine fname)
  let courseFolders = map (Path.combine fname) courseNames
      courses = map (\s -> EdxCourse
          { edxCourseContextId = if "course" `isPrefixOf` s then Text.pack s
                                 else "course-v1:" <> Text.pack s
          , edxCourseFriendlyName = Nothing}
        ) courseNames
  vals <- forM courses $ \c -> upsert c [EdxCourseContextId =. edxCourseContextId c]
  return . zip courseFolders . map entityKey $ vals


setExercises :: FilePath -> EdxCourseId -> DBEnv EdxResource [(FilePath, EdxResourceId)]
setExercises cpath courseId = do
  eNames <- liftIO $ listDirectory cpath >>= filterM (doesDirectoryExist . Path.combine cpath)
  let eFolders = map (Path.combine cpath) eNames
      exercises = map (\s -> EdxResource
          { edxResourceLink = Text.pack s
          , edxResourceFriendlyName = Nothing
          , edxResourceCourseId = courseId
          }
        ) eNames
  vals <- forM exercises $ \c -> upsert c [EdxResourceLink =. edxResourceLink c]
  return . zip eFolders . map entityKey $  vals



persistStory :: EdxResourceId
             -> FilePath -- path to folder with exercise done
             -> DBEnv OldStory [OldStoryId]
persistStory rId spath = do
  files <- liftIO $ listDirectory spath >>= filterM (doesFileExist . Path.combine spath)
  let images = map (Path.combine spath)
               $ filter (\x -> ".png" `isSuffixOf` x
                            || ".jpg" `isSuffixOf` x
                            || ".jpeg" `isSuffixOf` x
                            || ".gif" `isSuffixOf` x
               ) files
  hasDesc <- liftIO . doesFileExist $ Path.combine spath "description.txt"
  if not hasDesc
  then return []
  else liftIO (parseDiscussionTxt (Path.combine spath "description.txt")) >>= \mf -> case mf of
    Nothing -> return []
    Just storyF -> do
      imgPIds <- catMaybes <$> mapM saveImgPreview images
      mapM (insert . storyF rId) imgPIds
--      let r = Story
--            { storyResource = resourceId
--            , storyAuthor   = studentId
--            , storyImage    = previewId
--            , storyPlace    = placeId
--            , storyComment  = unTextarea $ tstoryComment story
--            , storyEdxOutcomeUrl = edxOutcomeUrl story
--            , storyEdxResultId   = edxResultId story
--            , storyCreationTime  = time
--            }
--      i <- insert r
--      return $ Entity i r
--      return undefined
  where
    saveImage name ctype content =
        insert $ Image name ctype content

    saveImgPreview filename = do
        ee <- liftIO $ try $
                 if "png" `isSuffixOf` filename
            then do
              i <- loadPngFile filename
              b <- savePngByteString i
              return (i,b,"image/png")
            else if "gif" `isSuffixOf` filename
            then do
              i <- loadGifFile filename
              b <- saveGifByteString i
              return (i,b,"image/gif")
            else do
              i <- loadJpegFile filename
              b <- saveJpegByteString 95 i
              return (i,b,"image/jpeg")
        case ee of
          Left e -> liftIO (print (e :: SomeException)) >> return Nothing
          Right (img, bytes, mime) -> fmap Just $ do
            preview <- liftIO $ do
              pimg <- newImage (previewSize, previewSize)
              (w,h) <- imageSize img
              let ssize  = min w h
                  spoint = ((w - ssize) `div` 2, (h-ssize) `div` 2)
                  dpoint = (0,0)
              copyRegionScaled spoint (ssize,ssize) img
                               dpoint (previewSize, previewSize) pimg
              savePngByteString pimg
            imgId <- saveImage (Text.pack $ Path.takeFileName filename) (Text.pack mime) bytes
            insert $ ImagePreview preview imgId



