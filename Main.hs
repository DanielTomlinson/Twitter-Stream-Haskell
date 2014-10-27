{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import Text.JSON.Yocto
import Web.Twitter.Conduit (stream, statusesFilterByTrack)
import Common
import Control.Lens ((^!), (^.), act)
import Data.Map ((!))
import Data.List (isInfixOf, or)
import Web.Twitter.Types

main :: IO ()
main = do
	let query = "london"
	T.putStrLn $ T.concat [ "Streaming Tweets that match \"", query, "\"..."]
	analyze query

analyze :: T.Text -> IO ()
analyze query = runTwitterFromEnv' $ do
	src <- stream $ statusesFilterByTrack query
	src C.$$+- CL.mapM_ (^! act (liftIO . process))

process :: StreamingAPI -> IO ()
process (SStatus s) = printStatus s
process s = return ()

parseStatus :: Status -> T.Text
parseStatus (s) = T.concat ["@", (userScreenName $ statusUser s), ": ", (statusText s)]

printStatus :: Status -> IO ()
printStatus (s) = T.putStrLn $ parseStatus s
