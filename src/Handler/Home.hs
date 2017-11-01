module Handler.Home where


import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import qualified Database.Esqueleto      as E
--import           Database.Esqueleto      ((^.))
import qualified Database.Persist.Sql    as PSQL

import Import

getHomeR :: Handler Html
getHomeR = lookupGetParam "r" >>= getAllHomes False . maybeToList . fmap E.toSqlKey . maybeInt

getOldHomeR :: Handler Html
getOldHomeR = lookupGetParam "r" >>= getAllHomes True . maybeToList . fmap E.toSqlKey . maybeInt


getExerciseR :: Text -> Handler Html
getExerciseR name = runDB (selectList [ EdxResourceParamKey ==. "linksuffix"
                                      , EdxResourceParamValue ==. name]
                                      [Desc EdxResourceParamId]
                          )
                >>= getAllHomes False . fmap (edxResourceParamResourceId . entityVal)


getAllHomes :: Bool -> [EdxResourceId] -> Handler Html
getAllHomes getOld resIds = do
    setUltDestCurrent
--    getAllStories <- runDB prepareGetStories
    getStoriesForTasks <- runDB prepareGetStoriesForTasks
    getStoriesForCourse <- runDB prepareGetStoriesForCourse
    let countStories = if getOld
                       then count ([] :: [Filter OldStory])
                       else count ([] :: [Filter Story])
    countStoriesForTasks <- runDB prepareCountStoriesForTasks
    countStoriesForCourse <- runDB prepareCountStoriesForCourse
    mmsg <- getMessage
    muser <- fmap entityVal <$> maybeAuth
    mcourseId <- fmap E.toSqlKey . maybeInt <$> lookupGetParam "c" :: Handler (Maybe (Key EdxCourse))
    l <- maybeLimit <$> lookupGetParam "l"
    o' <- maybeOffset <$> lookupGetParam "o"
    totalNumber <- runDB $ case (resIds, mcourseId) of
          ([], Just courseId) -> countStoriesForCourse courseId
          ([], Nothing) -> countStories
          (xs,_) -> countStoriesForTasks xs
    currentFolder <- case resIds of
           [] -> runDB $ (\s -> [shamlet|<p.headingline>#{fixemptyname s}|]) <$> getCById mcourseId
           (resId:_) -> runDB $ do
                mres <- get resId
                case mres of
                  Nothing -> (\s -> [shamlet|<p.headingline>#{fixemptyname s}|]) <$> getCById mcourseId
                  Just res -> do
                    mcourse <- get $ edxResourceCourseId res
                    return $ case (getCourseName mcourse, getResName mres) of
                      ("", "") -> [shamlet|<p.headingline>Edx user stories|]
                      ("", s)  -> [shamlet|<p.headingline>#{s}|]
                      (s, "")  -> [shamlet|<p.headingline>#{s}|]
                      (s,t)    -> [shamlet|<p.headingline style="margin-right: 1em;">
                                                #{s}
                                           <p.headingline>
                                                #{t} |]
    let hasAdminRights = isAdmin muser
        adminMarginLeft x = if hasAdminRights then "margin-left:" ++ show (x :: Double) ++ "em;" else ""
        courseRenHeading course = "Rename a course"
                               <> fromMaybe "" ((" " <>) <$> edxCourseFriendlyName course)
                               <> "<br/>(" <> edxCourseContextId course  <> ")"
        resRenHeading res = "Rename an exercise"
                               <> fromMaybe "" ((" " <>) <$> edxResourceFriendlyName res)
                               <> "<br/>(" <> edxResourceLink res  <> ")"
        p = (o' `div` l) + 1
        o = (p - 1) * l
        pages = zip ([1..]:: [Int]) [0,l..totalNumber]
        proute = if getOld then OldHomeR else HomeR
        pageLinkQ = case (resIds, mcourseId) of
          ([],Nothing) -> ""
          (_, Just courseId) -> "&c=" <> Text.pack (show $ E.fromSqlKey courseId)
          (resId:_,_) -> "&r=" <> Text.pack (show $ E.fromSqlKey resId)
        query = case (resIds, mcourseId) of
          ([],Nothing) -> (if getOld then getOldStories o l >>= mapM viewOldStory
                                        else getStories o l >>= mapM viewStory)
          ([],Just courseId) -> getStoriesForCourse courseId o l
          (xs,_) -> getStoriesForTasks xs o l
    (storedFiles, edxResources) <- runDB $ do
          a <- query
          b <- getResCourses
          return (a,b)

    defaultLayout $ do
        setTitle "EdX User Stories"
        $(widgetFile "home")
  where
    fixemptyname "" = "Edx user stories"
    fixemptyname s = s
    getCById (Just courseId) = getCourseName <$> get courseId
    getCById Nothing = return ""
    getCourseName mcourse = case mcourse of
      Nothing -> ""
      Just course -> fromMaybe (edxCourseContextId course) (edxCourseFriendlyName course)
    getResName mres = case mres of
      Nothing -> ""
      Just res -> fromMaybe (edxResourceLink res) (edxResourceFriendlyName res)



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

maybeInt :: Maybe Text -> Maybe Int64
maybeInt Nothing = Nothing
maybeInt (Just m) = case Text.decimal m of
    Left _ -> Nothing
    Right (n,_) -> Just n


getStories :: Int -> Int -> ReaderT SqlBackend Handler [Entity Story]
getStories offset limit = selectList [] [Desc StoryCreationTime, OffsetBy offset, LimitTo limit]

getOldStories :: Int -> Int -> ReaderT SqlBackend Handler [Entity OldStory]
getOldStories offset limit = selectList [] [Desc OldStoryCreationTime, OffsetBy offset, LimitTo limit]


getCourses :: ReaderT SqlBackend Handler [Entity EdxCourse]
getCourses = selectList [] [Asc EdxCourseFriendlyName, Asc EdxCourseContextId]

getResources :: Key EdxCourse -> ReaderT SqlBackend Handler [Entity EdxResource]
getResources courseId = selectList [EdxResourceCourseId ==. courseId] [Asc EdxResourceFriendlyName, Asc EdxResourceLink]


getResCourses :: ReaderT SqlBackend Handler [(Entity EdxCourse, [Entity EdxResource])]
getResCourses = fmap groupThen $ PSQL.rawSql query []
  where
    query = Text.unlines
          ["SELECT ??, ??"
          ,"FROM  edx_course, edx_resource"
          ,"WHERE edx_course.id = edx_resource.course_id"
          ,"  AND 0 < (SELECT count(t.x) FROM"
          ,"            (SELECT story.resource as x FROM story"
          ,"             WHERE edx_resource.id = story.resource"
          ,"             UNION ALL"
          ,"             SELECT old_story.resource FROM old_story"
          ,"             WHERE edx_resource.id = old_story.resource"
          ,"             ) t);"]
    groupThen [] = []
    groupThen ((c@(Entity i _),r):xs) = let (g, rest) = takeSame i xs
                           in (c, r:g) : groupThen rest
    takeSame _ [] = ([],[])
    takeSame a xss@((Entity c _,r):xs) | a /= c    = ([], xss)
                                       | otherwise = first (r:) $ takeSame a xs

--getResCourses :: ReaderT SqlBackend Handler [(Entity EdxCourse, [Entity EdxResource])]
--getResCourses = fmap groupThen . E.select $ E.from $ \(course `E.InnerJoin` resource) -> do
--    E.on $ resource ^. EdxResourceCourseId E.==. course ^. EdxCourseId
--    E.orderBy [ E.asc (course ^. EdxCourseFriendlyName), E.asc (course ^. EdxCourseContextId)
--              , E.asc (resource ^. EdxResourceFriendlyName), E.asc (resource ^. EdxResourceLink)]
--    return (course, resource)
--  where
--    groupThen [] = []
--    groupThen ((c@(Entity i _),r):xs) = let (g, rest) = takeSame i xs
--                           in (c, r:g) : groupThen rest
--    takeSame _ [] = ([],[])
--    takeSame a xss@((Entity c _,r):xs) | a /= c    = ([], xss)
--                                       | otherwise = first (r:) $ takeSame a xs
