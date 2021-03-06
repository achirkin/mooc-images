-----------------------------------------------------------------------------
--
-- Module      :  Handler.ImageUpload
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Handler.ImageUpload where

import Data.Conduit
import qualified Data.Conduit.Binary as CB
--import Data.Maybe (isJust)
import Data.CaseInsensitive (original)

import Data.Default
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8       as BSC
--import qualified Data.ByteString.Lazy as BSL
import Yesod
import Yesod.Default.Util
import Yesod.Core.Types

import Text.Blaze (ToMarkup)
--import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.Lazy as Base64L

import Import

import qualified Network.Wai as Wai (requestHeaders)
import Web.LTI
import Control.Monad.Trans.Except (runExceptT)


getImageUploadR :: Handler Html
getImageUploadR = postImageUploadR

postImageUploadR :: Handler Html
postImageUploadR = setupSession $ \isNewSession -> do
    ((res, widget), formEncType) <- runFormPost uploadForm
    html <- case res of
      FormFailure msgs -> uploadFormLayout $ do
        unless isNewSession $ showFormError msgs
        showFormWidget widget formEncType
      FormMissing -> uploadFormLayout $
        showFormWidget widget formEncType
      FormSuccess story -> do
        sessionvals <- getSession
        -- save story
        Entity _ s <- persistStory story (foldr getCustomParams [] $ Map.toList sessionvals)
        -- grade student
        app <- getYesod
        case (,) <$> storyEdxOutcomeUrl s <*> storyEdxResultId s of
           Nothing -> return ()
           Just (url,rId) -> do
             gr <- replaceResultRequest (appLTICredentials $ appSettings app) (Text.unpack url) rId 1.0 Nothing
             _ <- httpNoBody gr
             return ()
        -- finish
        deleteSession "resource_link_id"
        uploadFormLayout $
          showFormSuccess story
    -- setup all session params
    when isNewSession $ (fmap fst runRequestBody) >>= mapM_ saveCustomParams
    -- view html
    return html
  where
    saveCustomParams (k,v) = do
      case Text.stripPrefix "custom_" k of
        Nothing -> return ()
        Just key -> setSession ("storesession_" <> key) v
    getCustomParams (k,v) xs = case Text.stripPrefix "storesession_" k of
        Nothing -> xs
        Just key -> (key, Text.decodeUtf8 v):xs
    uploadFormLayout c = defaultLayout $ setTitle "Share your story" >> c
    showFormError :: [Text] -> Widget
    showFormError msgs = do
      errorWrapper <- newIdent
      toWidget [cassius|
        ##{errorWrapper} div
            background-color: #fcc;
            padding: 3px 5%;
            margin: 3px;
            font-variant: all-small-caps;
      |]
      [whamlet|
        <div ##{errorWrapper}>
          $forall msg <- msgs
            <div>#{msg}
      |]
    showFormWidget :: Widget -> Enctype -> Widget
    showFormWidget widget formEncType = do
--      addStylesheet $ StaticR awesomplete_css
      -- addScript
      toWidgetHead $
        [hamlet|
            <script src="@{StaticR js_jquery_2_2_3_min_js}" type="text/javascript">
            <script src="@{StaticR js_jquery_autocomplete_min_js}" type="text/javascript">
        |]
      [whamlet|
        <form method=post action=@{ImageUploadR} enctype=#{formEncType}>
          ^{widget}
      |]
    showFormSuccess :: TStory -> Widget
    showFormSuccess story = do
      successWrapper <- newIdent
      toWidget [cassius|
        ##{successWrapper}
            background-color: #ccf;
            padding: 5px 5%;
            margin: 5%;
            font-variant: all-small-caps;
      |]
      [whamlet|
        <div ##{successWrapper}>
          Thank you,#
          $case tstoryAuthor story
            $of Just author
              \ #{author},
            $of Nothing
          \ your story is uploaded
      |]

-- | Test if current session corresponds to resource_link_id
--   sent by LTI resource provider (edX).
--   Create a new session if needed,
--   otherwise fail.
setupSession :: (Bool -> Handler Html) -> Handler Html
setupSession continue = do
    msesResLink <- lookupSession "resource_link_id"
    postRL <- lookupPostParam "resource_link_id"
    getRL  <- lookupGetParam "resource_link_id"
    if msesResLink /= Nothing
       && (msesResLink == postRL || msesResLink == getRL)
    then continue False
    else do
        deleteSession "resource_link_id"
        yreq <- getRequest
        t_lti <- appLTICredentials . appSettings <$> getYesod
        eltiRequest <- runExceptT $ processYesodRequest t_lti yreq
        case eltiRequest of
          Left err -> do
            loggingRequest <- requestDebugInfo
            $(logWarn) $ "A user experienced an LTI error. His request was:\n"
               `Text.append` loggingRequest
            defaultLayout $ do
              setTitle "Error"
              [whamlet|<p>#{show err}|]
          Right _ -> do
            case postRL of
                Nothing -> defaultLayout $ do
                    setTitle "Error"
                    [whamlet|<p>Failed to check OAuth request from the LTI service consumer|]
                Just rlink -> do
                    setSession "resource_link_id" rlink
                    continue True


requestDebugInfo :: Handler Text
requestDebugInfo = do
    (rbody, rfiles) <- runRequestBody
    yreq <- getRequest
    wreq <- waiRequest
    return $ "\nRequest HEADERS:\n"
           % Text.unlines (map (\(p,v) -> Text.decodeUtf8 (original p) % ": " % Text.decodeUtf8 v) (Wai.requestHeaders wreq))
           % "\n"
           % "\nRequest GET parameters:\n"
           % Text.unlines (map (\(p,v) -> p % ": " % v) (reqGetParams yreq))
           % "\n"
           % "\nRequest POST parameters:\n"
           % Text.unlines (map (\(p,v) -> p % ": " % v) rbody)
           % "\n"
           % (if null rfiles then "" else "\nRequest contains "
                % Text.pack (show $ length rfiles) % " files.\n")
           % "\nRequest SESSION parameters:\n"
           % Text.pack (show $ reqSession yreq)
           % "\n\n"
  where
    infixl 5 %
    s % t = s `Text.append` t


uploadForm :: Html -> MForm Handler (FormResult TStory, Widget)
uploadForm extra = do
    -- set up ids of main div elements
    storyDataDiv  <- newIdent
    imgPreviewDiv <- newIdent
    topDiv    <- newIdent
    bottomDiv <- newIdent
    creditsDiv <- newIdent

    -- set up hidden field for edX user-related information
    (userIdRes,    userIdView    ) <- mreq hiddenField opts
      { fsName = Just "user_id"                 } Nothing
    (contextIdRes, contextIdView ) <- mreq hiddenField opts
      { fsName = Just "context_id"              } Nothing
    (resLinkRes,   resLinkView   ) <- mreq hiddenField opts
      { fsName = Just "resource_link_id"        } Nothing
    (outcomeUrlRes,outcomeUrlView) <- mopt hiddenField opts
      { fsName = Just "lis_outcome_service_url" } Nothing
    (resultIdRes,  resultIdView  ) <- mopt hiddenField opts
      { fsName = Just "lis_result_sourcedid"    } Nothing
    let edxInfoFields = do
          fvInput userIdView
          fvInput contextIdView
          fvInput resLinkView
          fvInput outcomeUrlView
          fvInput resultIdView

    -- set up all input
    (authorRes, authorView ) <- tryFillUserName userIdRes $ mopt textField opts
    (mimageRes, imageView  ) <- mopt fileField     reqs
       { fsAttrs = ("accept","image/*") : fsAttrs reqs} Nothing
    (countryRes,countryView)  <- mreq textField    reqs Nothing
    (placeRes,  placeView   ) <- mreq textField    reqs
       { fsAttrs = ("disabled","true") : fsAttrs reqs}  Nothing
    (commentRes,commentView) <- mreq textareaField
        reqs{ fsAttrs =
                ("onkeyup", "checkLength()"):("onchange", "checkLength()") : fsAttrs reqs
            , fsId = Just "storyComment"
            }
        Nothing
    (agreeRes,  agreeView  ) <- mreq checkBoxField reqs Nothing

    (_countryIdRes, countryIdView ) <- mreq hiddenField opts
      { fsName = Just "country_id"} (Nothing :: Maybe CountryId)
    (placeIdRes,   placeIdView   ) <- mreq hiddenField opts
      { fsName = Just "place_id"  } Nothing


    -- set up hidden fields for keeping image
    (imgFName, imgFType, imgBase64) <- case mimageRes of
        FormSuccess (Just fi) -> do
            fb <- runResourceT $ fileSource fi $$ CB.sinkLbs
            return ( Just $ fileName fi
                   , Just $ fileContentType fi
                   , Just . LText.decodeUtf8 $ Base64L.encode fb
                   )
        _ -> return (Nothing, Nothing, Nothing)
    (inameRes, inameView) <- addHiddenValueHolder imgFName
    (itypeRes, itypeView) <- addHiddenValueHolder imgFType
    (idataRes, idataView) <- addHiddenValueHolder imgBase64

    let -- take either current image or previous (if any)
        imageRes = case ( mimageRes
                        , makeFileInfo inameRes itypeRes idataRes) of
            (FormSuccess (Just x), _)       -> FormSuccess x
            (FormSuccess Nothing , Just x ) -> FormSuccess x
            (FormSuccess Nothing , Nothing) -> FormFailure ["Value is required"]
            (FormMissing         , Just x ) -> FormSuccess x
            (FormMissing         , Nothing) -> FormFailure ["Value is required"]
            (FormFailure _       , Just x ) -> FormSuccess x
            (FormFailure errs    , Nothing) -> FormFailure errs

        setErrMsg s v = case v of
            FormFailure (m:ms) ->
                FormFailure ((s `Text.append` ": " `Text.append` m):ms)
            x -> x

        mustAgree s = case agreeRes of
            FormSuccess False ->
                FormFailure ["You must agree the terms of use (check the checkbox)"] *> s
            x -> x *> s

        storyRes' =
          if all not --  test if we filled anything
            [ hasValue imageRes
            , hasValue placeRes
            , hasValue countryRes
            , hasValue commentRes]
          then FormMissing
          else mustAgree $ TStory
            <$> setErrMsg "Error in edX-provided field \"user_id\"" userIdRes
            <*> setErrMsg "Error in edX-provided field \"context_id\"" contextIdRes
            <*> setErrMsg "Error in edX-provided field \"resource_link_id\"" resLinkRes
            <*> setErrMsg "Error in edX-provided field \"lis_outcome_service_url\"" outcomeUrlRes
            <*> setErrMsg "Error in edX-provided field \"lis_result_sourcedid\"" resultIdRes
            <*> setErrMsg "Error in the field \"author\"" authorRes
            <*> setErrMsg "Error in the field \"image file\"" imageRes
            <*> setErrMsg "Error in the field \"place\"" placeIdRes
            <*> setErrMsg "Error in the field \"commentary\"" commentRes

        -- insert the image encoded data into the page later
        imageBytes = case (itypeRes, idataRes) of
          (Just typ, Just dat) -> Just $ "data:"
                `Text.append` typ
                `Text.append` ";base64,"
                `Text.append` (LText.toStrict dat)
          _ -> Nothing

    storyRes <- testPlaceExist placeIdRes storyRes'

    return (storyRes, $(widgetFileNoReload def "imageUpload"))
  where
    reqs = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [("required","required")]
        }
    opts = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = []
        }
    makeFileInfo (Just iname)
                 (Just itype)
                 (Just idata) = Just $ FileInfo
        { fileName = iname
        , fileContentType = itype
        , fileSourceRaw = CB.sourceLbs . Base64L.decodeLenient
                                    $ LText.encodeUtf8 idata
        , fileMove = \_ -> return ()
        }
    makeFileInfo _ _ _ = Nothing


-- | Returns default value if it is given,
--   otherwise returns input value
addHiddenValueHolder :: (PathPiece p, ToMarkup p)
                     => Maybe p
                     -> MForm Handler (Maybe p, Widget)
addHiddenValueHolder mx = do
  xName <- newIdent
  (xRes', xView) <- mopt hiddenField FieldSettings
    { fsLabel   = ""
    , fsTooltip = Nothing
    , fsId      = Nothing
    , fsName    = Just xName
    , fsAttrs   = []
    }  Nothing
  let xRes = case mx of
        Nothing -> case xRes' of
           FormSuccess (Just x) -> Just x
           _                    -> Nothing
        Just x  -> Just x
  return . (,) xRes $ case xRes of
    Just x ->
        [whamlet|<input type="hidden" ##{fvId xView} name="#{xName}" value="#{x}">|]
    _ -> return ()

hasValue :: FormResult a -> Bool
hasValue (FormSuccess _) = True
hasValue (FormFailure _) = False
hasValue FormMissing = False


testPlaceExist :: FormResult PlaceId
               -> FormResult a
               -> MForm Handler (FormResult a)
testPlaceExist (FormSuccess pid) v
  = fmap (test v) . lift . runDB $ do
    mplace   <- get pid
    return . isJust $ mplace
  where
    msg = "Invalid place value! Re-enter country and place fields."
    test (FormSuccess _) False = FormFailure [msg]
    test  FormMissing    False = FormMissing
    test (FormFailure e) False = FormFailure $ msg:e
    test x               True  = x
testPlaceExist _ v = return v


tryFillUserName :: FormResult Text
                -> (Maybe (Maybe Text) -> MForm Handler (FormResult (Maybe Text),w))
                -> MForm Handler (FormResult (Maybe Text), w)
tryFillUserName FormMissing        f = f Nothing
tryFillUserName (FormFailure _)    f = f Nothing
tryFillUserName (FormSuccess euid) f = do
  mmval <- lift . runDB $ do
    muser <- getBy $ EdxUserId euid
    return $ case muser of
      Nothing -> Nothing
      Just (Entity _ user) -> Just $ studentName user
  (res',w) <- f mmval
  let res = case (res', mmval) of
        (_, Nothing) -> res'
        (_, Just Nothing) -> res'
        (r@(FormSuccess (Just _)), _) -> r
        (FormSuccess Nothing, Just (Just x)) -> FormSuccess $ Just x
        (FormFailure _, Just (Just x)) -> FormSuccess $ Just x
        (FormMissing, Just (Just x)) -> FormSuccess $ Just x
  return (res,w)
