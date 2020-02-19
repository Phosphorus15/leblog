{-# LANGUAGE OverloadedStrings #-}
module Side where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import Data.String
import qualified Data.Text as T
import System.IO
import Data.Aeson
import Data.HashMap.Strict

-- Sendgrid mail api integration
sendmail :: String -> String -> IO String
sendmail value code = do
    token <- readFile "sendgrid.token"; -- This token has been absolutely hidden
    let req = setRequestBody (fromString $ formatMail value code) $ 
              setRequestHeader "Authorization" [fromString ("Bearer " ++ (T.unpack $ T.strip $ fromString $ token))] $
              setRequestHeader "Content-Type" ["application/json"] $
              "POST https://api.sendgrid.com/v3/mail/send"
    res <- httpBS req
    pure $ B8.unpack $ getResponseBody res

-- Google recaptcha verification
recaptcha :: String -> IO Bool
recaptcha value = do
    token <- readFile "recaptcha.server"
    res <- httpBS $ fromString ("GET https://recaptcha.net/recaptcha/api/siteverify?secret=" ++ token ++ "&response=" ++ value)
    case (decode $ fromString $ B8.unpack $ getResponseBody res) :: Maybe Value of
        Just(Object jsonmap) -> case Data.HashMap.Strict.lookup "success" jsonmap of
                                    Just b -> case b of
                                                Bool boolean -> pure boolean
                                                _ -> pure False
                                    _ -> pure False
        _ -> pure False

recaptchaRaw :: String -> IO String
recaptchaRaw value = do
    token <- readFile "recaptcha.server"
    res <- httpBS $ fromString ("GET https://recaptcha.net/recaptcha/api/siteverify?secret=" ++ token ++ "&response=" ++ value)
    pure $ B8.unpack $ getResponseBody res

formatMail :: String -> String -> String
formatMail tomail code = "{\"personalizations\": [{\"to\": [{\"email\": \"" ++ tomail ++ "\"}]}],\"reply-to\": {\"email\", \"phosphophate@gmail.com\"},\"from\": {\"email\": \"no-reply@leblog.com\"},\"subject\": \"Your verification code at LeBlog\",\"content\": [{\"type\": \"text/html\", \"value\": \"Hi, there !<br/>Please click the link below to complete your registration: <br/> <a href=\\\"http://leblog.me/re/"++ code ++"\\\">Link</a> .\"}]}"
