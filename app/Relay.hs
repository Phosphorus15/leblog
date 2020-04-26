{-# LANGUAGE OverloadedStrings #-}
module Relay where

import Network.Wai
import Data.String
import qualified Data.List as L
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.Split as Split

doRelay :: Request -> Request
doRelay req = case L.find (\header -> (fst header == CI.mk "Host")) (requestHeaders req) of
                    (Just host) -> let hostArray = Split.splitOn "." $ B8.unpack $ snd host in
                                    if length hostArray > 2 then req { pathInfo = map fromString [ "u", hostArray !! 0 ] ++ (pathInfo req) } else req
                    Nothing -> req

userRelay :: Middleware
userRelay app = \req -> \res -> app (req {isSecure = False}) res
