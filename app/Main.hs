{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString.Char8 as BC8
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Status (status303,status200)
import System.Environment (getArgs)
import Data.Maybe
import Data.Char (toLower)

main :: IO ()
main = do
  args <- getArgs
  fp <- case args of
    [x] -> return x
    _ -> fail "should be called with one argument, a file path"
  run 8983 (app fp)

app :: FilePath -> Application
app fp req respond = do
  pairs <- id
    . mapMaybe (\x -> case Text.splitOn " " x of
      [k,v] -> Just (encodeUtf8 (Text.toLower k),encodeUtf8 v)
      _ -> Nothing
    ) 
    . map Text.strip 
    . Text.lines 
    <$> Text.readFile fp
  let problem = case lookup "from" (queryString req) of
        Nothing -> responseLBS status200 [] "Bad Code"
        Just Nothing -> responseLBS status200 [] "Bad Code"
        Just (Just url) -> responseLBS status303 [("Location",url)] ""
  respond $ case lookup "code" (queryString req) of
    Nothing -> problem
    Just m -> case m of
      Nothing -> problem
      Just code -> case lookup (BC8.map toLower code) pairs of
        Nothing -> problem
        Just url -> responseLBS status303 [("Location",url)] ""

