{-# LANGUAGE OverloadedStrings 		#-}
{-# LANGUAGE ExtendedDefaultRules   #-}

import Hakit
import Hakit.Http
import qualified Data.Text as T

-- Simply displays hello world.
ex p = startServer p $ \_ -> return $ setBody "Hello world." resp

-- Sets a cookie.
ex1 p = startServer p $ \_ -> return . setCookie "Hello" "World" $ setBody "Cookie set." resp

-- Deletes a cookie.
ex2 p = startServer p $ \_ -> return . setCookie "Hello" "" $ setBody "Cookie unset." resp

-- Display cookie
ex3 p = do
	let handler req = do
			let coo = get "Hello" $ cookies req
			return $ setBody (T.concat ["Req cookie: ", toString coo]) resp
	startServer p handler

-- Shows an example of how to route based on request path.
ex4 p = do
	let handler req = do
			let r = case path req of
					[]					-> 	let coo = get "Hello" $ cookies req
											in setBody (T.concat ["Hello world. Req cookie: ", toString coo]) resp
					["setCookie", v]	->	setBody "Cookie set" $ setCookie "Hello" v resp
					["delCookie"]		-> 	setBody "Cookie unset" $ setCookie "Hello" "" resp
			return r
	startServer p handler

-- For testing POST.
ex5 p = startServer p $ \req -> return $ setBody (toJSON $ params req) resp