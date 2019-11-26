{-# LANGUAGE OverloadedStrings #-}
module Side where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import Data.String
import qualified Data.Text as T
import System.IO

sendmail :: String -> String -> IO Int
sendmail value code = do
    token <- readFile "sendgrid.token";
    let req = setRequestBody (fromString $ formatMail value code) $ 
              setRequestHeader "Authorization" [fromString ("Bearer " ++ (T.unpack $ T.strip $ fromString $ token))] $
              setRequestHeader "Content-Type" ["application/json"] $
              "POST https://api.sendgrid.com/v3/mail/send"
    res <- httpBS req
    pure (getResponseStatusCode res)

formatMail :: String -> String -> String
formatMail tomail code = "{\"personalizations\": [{\"to\": [{\"email\": \"" ++ tomail ++ "\"}]}],\"reply-to\": {\"email\", \"phosphophate@gmail.com\"},\"from\": {\"email\": \"no-reply@leblog.com\"},\"subject\": \"Your verification code at LeBlog\",\"content\": [{\"type\": \"text/html\", \"value\": \"Hi, there !<br/>Your verification code for registry is <code>"++ code ++"</code> .\"}]}"