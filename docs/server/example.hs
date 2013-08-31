{-# LANGUAGE OverloadedStrings #-}

import Hakit
import Hakit.Server

import qualified Data.ByteString.Lazy as LBS

main = startHttp 7070 $ \_ -> return $ quickText "html" "Hello"