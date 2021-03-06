module Foundation where

--import Control.Concurrent.STM.TChan


import Data.Conduit
import Data.Conduit.Binary
import qualified Data.ByteString.Lazy as BSL
import Graphics.GD.ByteString.Lazy
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import qualified Data.Text as Text
import Text.Jasmine         (minifym)
--import Yesod.Auth.OpenId    (authOpenId, IdentifierType (Claimed))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
--import qualified Data.CaseInsensitive as CI
--import qualified Data.Text.Encoding as TE

import Import.NoFoundation
import Yesod.Auth.LdapNative


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
--    , appWSChan      :: TChan Text
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
--        master <- getYesod

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <- widgetToPageContent $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (RenameCourseR _) _ = testAdmin
    isAuthorized (RenameResourceR _) _ = testAdmin
    isAuthorized (DeleteStoryR _) _ = testAdmin
    isAuthorized (DeleteOldStoryR _) _ = testAdmin
--    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger
    maximumContentLength _ (Just ImageUploadR) = Just 20000000
    maximumContentLength _ _ = Just 2000000

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool


ldapConf :: LdapAuthConf
ldapConf = setHost (Secure "isla.ethz.ch") $ setPort 636
  $ mkLdapConf Nothing "cn=users,dc=isla,dc=ethz,dc=ch"

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
      x <- insertBy $ User (credsIdent creds) False
      return $ Just $ case x of
        Left (Entity userid _) -> userid -- newly added user
        Right userid -> userid -- existing user

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins _ = [authLdapWithForm ldapConf $ \loginR -> getMessage >>= \mmsg -> $(widgetFile "login") ]

    authHttpManager = appHttpManager

instance YesodAuthPersist App

isAdmin :: Maybe User -> Bool
isAdmin Nothing = False
isAdmin (Just (User _ a)) = a

testAdmin :: Handler AuthResult
testAdmin = (\mu -> if isAdmin mu then Authorized else Unauthorized "Need admin rights to do this.")
        . fmap entityVal <$> maybeAuth

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding


data TStory = TStory
    { edxUserId     :: Text
    , edxContextId  :: Text
    , edxResLink    :: Text
    , edxOutcomeUrl :: Maybe Text
    , edxResultId   :: Maybe Text
    , tstoryAuthor  :: Maybe Text
    , tstoryImage   :: FileInfo
    , tstoryPlace   :: PlaceId
    , tstoryComment :: Textarea
    }

-- | size of preview image in pixels
previewSize :: Int
previewSize = 400

persistStory :: TStory -> [(Text,Text)] -> Handler (Entity Story)
persistStory story storesessionmap = runDB $ do
    studentId  <- saveStudent
    resourceId <- saveResource
    forM_ storesessionmap $ \(key,val) ->
      void (upsert (EdxResourceParam resourceId key val) [EdxResourceParamValue =. val])
    previewId  <- saveImgPreview
    let placeId = tstoryPlace story
    time <- liftIO getCurrentTime
    let r = Story
          { storyResource = resourceId
          , storyAuthor   = studentId
          , storyImage    = previewId
          , storyPlace    = placeId
          , storyComment  = unTextarea $ tstoryComment story
          , storyEdxOutcomeUrl = edxOutcomeUrl story
          , storyEdxResultId   = edxResultId story
          , storyCreationTime  = time
          }
    i <- insert r
    return $ Entity i r
  where
    saveStudent = do
      mEstudent <- getBy . EdxUserId $ edxUserId story
      case (mEstudent, tstoryAuthor story) of
       (Nothing, mauthor) -> insert Student
         { studentEdxUserId = edxUserId story
         , studentName      = mauthor
         }
       (Just (Entity stId _), Nothing) -> return stId
       (Just (Entity stId _), Just name) ->
            stId <$ update stId [StudentName =. Just name]

    saveCourse mEcourse = case mEcourse of
        Nothing -> insert $ EdxCourse (edxContextId story) Nothing
        Just (Entity i _) -> return i

    saveResource = do
      mEresource <- getBy . EdxResLink $ edxResLink story
      case mEresource of
        Nothing -> do
            mEcourse <- getBy . EdxContextId $ edxContextId story
            cId <- saveCourse mEcourse
            insert $ EdxResource (edxResLink story) cId Nothing
        Just (Entity i _) -> return i

    saveImage name ctype content =
        insert $ Image name ctype content

    saveImgPreview = do
        fb <- runResourceT $ fileSource (tstoryImage story) $$ sinkLbs
        let mime = fileContentType $ tstoryImage story
        preview <- liftIO $ do
          pimg <- newImage (previewSize, previewSize)
          img <- if "jpeg" `Text.isSuffixOf` mime
            then loadJpegByteString fb
            else if "png" `Text.isSuffixOf` mime
            then loadPngByteString fb
            else if "gif" `Text.isSuffixOf` mime
            then loadGifByteString fb
            else return pimg  -- TODO: make a proper error generation.
          (w,h) <- imageSize img
          let ssize  = min w h
              spoint = ((w - ssize) `div` 2, (h-ssize) `div` 2)
              dpoint = (0,0)
          copyRegionScaled spoint (ssize,ssize) img
                           dpoint (previewSize, previewSize) pimg
          savePngByteString pimg
        let content = BSL.toStrict fb
            ctype   = mime
            name    = fileName $ tstoryImage story
        imgId <- saveImage name ctype content
        insert $ ImagePreview (BSL.toStrict preview) imgId




