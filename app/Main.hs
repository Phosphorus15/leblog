{-# LANGUAGE OverloadedStrings #-}
import Web.Spock
import qualified Page
import qualified Renderer
import Web.Spock.Config
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Static
import Data.Pool
import Data.String
import Data.Aeson (ToJSON(toJSON), object, (.=),Value)
import Database.PostgreSQL.Simple.FromRow
import Data.Text.Lazy as Lazy
import Control.Monad.IO.Class
import qualified Network.HTTP.Types.Status as Status
import qualified Data.Text as T
import Data.UnixTime
import CMarkGFM

type AppAction a = SpockActionCtx () Connection AppSession AppState a

data AppState = EmptyState
data AppSession = EmptySession

data BlogPost = BlogPost { postId :: Int, postData :: String, date :: Int, poster :: String } deriving Show

data UserToken = UserToken { uid :: Int, secret :: String, name :: String, privilege :: Int } deriving Show

instance FromRow BlogPost where
    fromRow = BlogPost <$> field <*> field <*> field <*> field

instance FromRow UserToken where
    fromRow = UserToken <$> field <*> field <*> field <*> field

instance ToJSON BlogPost where
    toJSON (BlogPost pid content date poster) = object [ "id" .= pid, "data" .= (commonmarkToHtml [] [] $ fromString content), "date" .= date, "poster" .= poster ]

main :: IO ()
main = do
    pool<-createPool (connect (ConnectInfo "localhost" 5432 "phosphorus15" "12345" "blog") ) close 1 10 10;
    spockCfg <- defaultSpockCfg EmptySession (PCPool pool) EmptyState
    runSpock 8080 (spock spockCfg app)

app :: SpockM Connection AppSession AppState ()
app = do
    middleware (staticPolicy (addBase "static"))
    get root $ html $ Lazy.toStrict Page.staticMain
    get "posts" $ do
      xs<-runQuery $ \conn -> 
        query_ conn  "select id,data,date,poster from posts"
      json (xs::[BlogPost])
    get "post" $ html $ Lazy.toStrict Page.staticPost
    post "post" $ do
        requestParam <- parsePost;
        case requestParam of
            (Nothing, _) -> setStatus Status.status400 >> text "Missing content."
            (_, Nothing) -> setStatus Status.status400 >> text "Missing secret code."
            (Just content, Just secret) -> do
                xs <-runQuery $ \conn -> 
                    query conn  "select uid,secret,username,privilege from users where secret = ?" (Only secret);
                case xs :: [UserToken] of
                    [] -> setStatus Status.status400 >> text "Illegal secret code."
                    (user:xs) -> do
                        timestamp <- liftIO getUnixTime >>= return . show . utSeconds;
                        _ <- runQuery $ \conn -> 
                            execute conn  "insert into posts (data, date, poster) values(?, ?, ?)" (content, read timestamp :: Int , name user);
                        text "ok"


parsePost :: MonadIO m => ActionCtxT ctx m (Maybe Text, Maybe Text)
parsePost = do
    content <- param "content";
    secret <- param "secret";
    pure (content, secret)
