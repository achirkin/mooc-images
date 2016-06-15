module Handler.Home where


import qualified Data.Text.Read as Text
import Import


getHomeR :: Handler Html
getHomeR = do
    l <- maybeLimit <$> lookupGetParam "l"
    o' <- maybeOffset <$> lookupGetParam "o"
    totalNumber <- runDB $ count ([] :: [Filter Story])
    let p = o' `div` l
        o = p * l
        pages = zip ([0..]:: [Int]) [0,l..totalNumber]
        proute = HomeR
    storedFiles <- runDB $ getStories o l >>= mapM viewStory

    defaultLayout $ do
        setTitle "EdX User Stories"
        $(widgetFile "home")


getStories :: Int -> Int -> ReaderT SqlBackend Handler [Entity Story]
getStories offset limit = selectList [] [Desc StoryCreationTime, OffsetBy offset, LimitTo limit]

getOldStories :: Int -> Int -> ReaderT SqlBackend Handler [Entity OldStory]
getOldStories offset limit = selectList [] [Desc OldStoryCreationTime, OffsetBy offset, LimitTo limit]


getOldHomeR :: Handler Html
getOldHomeR = do
    l <- maybeLimit <$> lookupGetParam "l"
    o' <- maybeOffset <$> lookupGetParam "o"
    totalNumber <- runDB $ count ([] :: [Filter OldStory])
    let p = o' `div` l
        o = p * l
        pages = zip ([0..]:: [Int]) [0,l..totalNumber]
        proute = OldHomeR
    storedFiles <- runDB $ getOldStories o l >>= mapM viewOldStory

    defaultLayout $ do
        setTitle "EdX User Stories"
        $(widgetFile "home")



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
