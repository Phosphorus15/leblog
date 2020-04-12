{-# LANGUAGE OverloadedStrings #-}
import Web.Spock
import qualified Page
import qualified Renderer
import qualified Side
import Web.Spock.Config
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Static
import Data.Pool
import Data.String
import Data.Aeson (ToJSON(toJSON), object, (.=),Value)
import Data.List (intercalate)
import Database.PostgreSQL.Simple.FromRow
import Data.Text.Lazy as Lazy
import Control.Monad.IO.Class
import qualified Network.HTTP.Types.Status as Status
import qualified Data.Text as T
import Data.UnixTime
import CMarkGFM
import qualified Text.HTML.SanitizeXSS as S
import qualified Text.Read as R
import qualified Control.Lens as L
import Control.Monad.IO.Class
import Data.UUID (toString)
import Data.UUID.V1

type AppAction a = SpockActionCtx () Connection AppSession AppState a

data AppState = EmptyState
data AppSession = EmptySession

data BlogPost = BlogPost { postId :: Int, title :: String, postData :: String, date :: Int, poster :: String } deriving Show

data UserToken = UserToken { uid :: Int, secret :: String, name :: String, privilege :: Int } deriving Show

data UserInfo = UserInfo { id :: Int, username :: String, mail :: String}

data LoginInfo = LoginInfo { lid :: Int, valid :: Int, session :: String }

instance FromRow BlogPost where
    fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field

instance FromRow UserToken where
    fromRow = UserToken <$> field <*> field <*> field <*> field

instance FromRow UserInfo where
    fromRow = UserInfo <$> field <*> field <*> field

instance FromRow LoginInfo where
    fromRow = LoginInfo <$> field <*> field <*> field

instance ToJSON BlogPost where
    toJSON (BlogPost pid title content date poster) 
        = object [ "id" .= pid, "title" .= title , "data" .= ( S.sanitizeBalance $ commonmarkToHtml [] [] $ fromString content), "date" .= date, "poster" .= poster ]

main :: IO ()
main = do
    init <- connect $ ConnectInfo "localhost" 5432 "phosphorus15" "12345" "blog";
    _ <- execute_ init "create table if not exists users (uid SERIAL PRIMARY KEY,secret character varying(512),username character varying(128),mail character varying(128),rkey character varying(128) ,privilege integer, ty integer);";
    _ <- execute_ init "create table if not exists posts (id SERIAL PRIMARY KEY,title character varying(256),data character varying(16384),date integer,poster character varying(128));";
    _ <- execute_ init "create table if not exists login_status(uid integer, valid integer, session character varying(256))"
    pool <- createPool ( connect $ ConnectInfo "localhost" 5432 "phosphorus15" "12345" "blog" ) close 1 10 10;
    spockCfg <- defaultSpockCfg EmptySession (PCPool pool) EmptyState
    runSpock 8080 (spock spockCfg app)

-- Invokes when a 4xx status should be responsed
panic :: T.Text -> AppAction()
panic err = setStatus Status.status400 >> text err

app :: SpockM Connection AppSession AppState ()
app = do
    middleware (staticPolicy (addBase "static"))
    get root $ redirect "index.html"
    get "posts" requestPostsAction
    get "post" $ html $ Lazy.toStrict Page.staticPost
   -- get "register" $ html $ Lazy.toStrict Page.staticRegister
   -- get "login" $ html $ Lazy.toStrict Page.staticLogin
    get ("u" <//> var) $ \user -> do
        html $ Lazy.toStrict $ Page.dynamicUser user
    get ("p" <//> var) showPostAction
    get ("re" <//> var) mailRegisterAction
    post "post" registerAction
    post "register" registerAction
    post "login" loginAction
    hookAny GET $ \path -> redirect $ fromString ("/#/" ++ (Data.List.intercalate "/" $ Prelude.map T.unpack path)) -- checkLogin >>= (\t -> text $ "Page not found. \n" <> fromString (show t));

-- Returns the uid of current user according to session id
checkLogin :: AppAction (Maybe Int)
checkLogin = do session <- getSessionId
                status <- runQuery $ \conn -> query conn "select * from login_status where session = ?" (Only session)
                case (status :: [LoginInfo]) of
                    [] -> pure Nothing
                    (l:_) -> pure $ Just $ lid l

requestPostsAction :: AppAction ()
requestPostsAction = do
    (select, pid) <- parsePostRequest
    xs<-runQuery $ \conn -> 
      case (select, pid) of
        (Nothing, Nothing) -> query_ conn  "select id,title,data,date,poster from posts order by date desc;"
        (Just username, _) -> query conn "select id,title,data,date,poster from posts where poster=? order by date desc;" (Only username)
        (_, Just pid) -> query conn "select id,title,data,date,poster from posts where id=? order by date desc;" (Only pid)
    json (xs::[BlogPost])

postAction :: AppAction ()
postAction = do
    requestParam <- parsePost;
    case requestParam of
        (Just content, Just secret, Just title) -> do
            xs <-runQuery $ \conn ->
                query conn  "select uid,secret,username,privilege from users where secret = ?" (Only secret);
            case xs :: [UserToken] of
                [] -> panic "Illegal secret code."
                (user:xs) -> do
                    timestamp <- liftIO getUnixTime >>= return . show . utSeconds;
                    _ <- runQuery $ \conn ->
                        execute conn  "insert into posts (data, date, poster, title) values(?, ?, ?, ?)" (content, read timestamp :: Int , name user, title);
                    redirect "/"
        _ -> panic "Missing params."

uuidFallback :: Maybe String -> String -> String
uuidFallback Nothing mail = mail
uuidFallback (Just uuid) _ = uuid

registerAction :: AppAction ()
registerAction = do
    (recaptcha, email, username, pwd) <- parseRegisterRequest;
    case (recaptcha, email, username, pwd) of
        (Just token, Just mail, Just name, Just password) -> do response <- liftIO $ Side.recaptcha $ T.unpack $ Lazy.toStrict token;
                                                                if response then let mailstr = T.unpack $ Lazy.toStrict mail in
                                                                    do uuid <- fmap (fmap toString) $ liftIO nextUUID
                                                                       retval <- liftIO $ Side.sendmail mailstr $ uuidFallback uuid mailstr
                                                                       _ <- runQuery $ \conn ->
                                                                           execute conn "insert into users (secret, username, mail, rkey, privilege, ty) values (?, ?, ?, ?, ?, ?)" (password, name, mail, uuidFallback uuid mailstr, 1 :: Int, -1 :: Int)
                                                                       text "Verification mail has been sent, please follow the mail to complete your registration"
                                                                else panic "Recaptcha failed."
        _ -> panic "Missing field or recaptcha failed."

loginAction :: AppAction ()
loginAction = do
    (recaptcha, username, pwd) <- parseLoginRequest;
    case (recaptcha, username, pwd) of
        (Just token, Just name, Just password) -> do response <- liftIO $ Side.recaptcha $ T.unpack $ Lazy.toStrict token;
                                                     if response then do users <- runQuery $ \conn ->
                                                                                 query conn "select uid, username, secret, privilege from users where username = ? and secret = ?" (name, password)
                                                                         case (users :: [UserToken]) of
                                                                             [] -> panic "Incorrect login identities"
                                                                             (u:xs) -> do session <- getSessionId
                                                                                          _ <- runQuery $ \conn->
                                                                                              execute conn "insert into login_status values (?, ?, ?)" (uid u, 1 :: Int, session)
                                                                                          text "login success"
                                                     else panic "Recaptcha failed"

mailRegisterAction :: Text -> AppAction ()
mailRegisterAction registerKey = do
    xs <- runQuery $ \conn ->
        query conn "select uid, username, mail from users where rkey = ?" (Only registerKey)
    case xs :: [UserInfo] of
        [] -> panic "Illegal request link."
        (user:xs) -> do
            _ <- runQuery $ \conn ->
                execute conn "update users set ty = 1 where rkey = ?" (Only registerKey)
            text "Registration complete"

showPostAction :: Text -> AppAction ()
showPostAction pid = do
    case R.readMaybe $ T.unpack $ Lazy.toStrict pid :: Maybe Int of
        Just pid -> do
            posts <- runQuery $ \conn ->
                query conn "select * from posts where id = ?" [pid :: Int];
            case posts :: [BlogPost] of
                [] -> setStatus Status.status404 >> text "Post id not found."
                (post:xs) -> do
                    html $ Lazy.toStrict $ Page.dynamicPost pid (poster post)
        Nothing -> panic "Invalid post request."

parsePost :: MonadIO m => ActionCtxT ctx m (Maybe Text, Maybe Text, Maybe Text)
parsePost = do
    content <- param "content";
    secret <- param "secret";
    title <- param "title";
    pure $ L.over L.each mapEmpty (content, secret, title)

mapEmpty :: Maybe Text -> Maybe Text
mapEmpty Nothing = Nothing
mapEmpty (Just t) = if (Lazy.length $ strip t) == 0 then Nothing else Just t

parsePostRequest :: MonadIO m => ActionCtxT ctx m (Maybe Text, Maybe Int)
parsePostRequest = do
    username <- param "user";
    pid <- param "id";
    pure (mapEmpty username, pid)

parseRegisterRequest :: MonadIO m => ActionCtxT ctx m (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
parseRegisterRequest = do
    email <- param "mail";
    pwd <- param "pwd";
    username <- param "username";
    recaptcha <- param "g-recaptcha-response";
    pure (recaptcha, email, username, pwd)

parseLoginRequest :: MonadIO m => ActionCtxT ctx m (Maybe Text, Maybe Text, Maybe Text)
parseLoginRequest = do
    pwd <- param "pwd";
    username <- param "username";
    recaptcha <- param "g-recaptcha-response";
    pure (recaptcha, username, pwd)
