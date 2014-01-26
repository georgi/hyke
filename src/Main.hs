{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import System.Environment
import System.FilePath.Posix
import Data.Time.Clock
import Data.List.Split

import Types
import Renderer

readPost :: FilePath -> IO Post
readPost path = do
  text <- readFile path
  case splitOn "_" path of
    [year, month, day, _] ->
      return Post {
        postYear = read year,
        postMonth = read month,
        postDay = read day,
        postFile = path,
        postText = text
    }
    _ -> error $ "path not right: " ++ path

findFiles :: FilePath -> IO [FilePath]
findFiles path = do
  isFile <- doesFileExist path
  if isFile then
      return [path]
    else do
      entries <- getEntries path
      let paths = map (\ entry -> path ++ "/" ++ entry) entries
      files <- mapM findFiles paths
      return $ map normalise $ concat files
  where
    getEntries filepath = do
      contents <- getDirectoryContents filepath
      return $ filter ((/= '.') . head) contents

writeFeed :: Blog -> [Post] -> IO ()
writeFeed blog posts = do
  time <- getCurrentTime
  putStrLn path
  writeFile path $ atomFeed blog posts time
  where path = blogFolder blog ++ "/atom.xml"

writeIndex :: Blog -> Page -> [Post] -> IO ()
writeIndex _ _ [] = return ()
writeIndex blog (page,pageSize) posts = do
  putStrLn path
  writeFile path $ renderPostList blog posts' nextPage
  writeIndex blog (page + 1, pageSize) $ drop pageSize posts
  where posts' = take pageSize posts
        nextPage = fileForPage $ page + 1
        path = blogFolder blog ++ "/" ++ fileForPage page
        fileForPage 0 = ""
        fileForPage 1 = "index.html"
        fileForPage p = "page-" ++ show p ++ ".html"

writePost :: Blog -> Post -> IO ()
writePost blog post = do
  writeFile path text
  putStrLn path
  where path = blogFolder blog ++ "/" ++ postLink post
        text = renderPostPage blog post

writeBlog :: Blog -> IO ()
writeBlog blog = do
  posts <- findFiles "./" >>= mapM readPost
  mapM_ (writePost blog) posts
  writeIndex blog (1,3) $ reverse posts
  writeFeed blog $ take 10 $ reverse posts

getBlog :: IO Blog
getBlog = do
  folder <- getEnv "BLOG_FOLDER"
  uri <- getEnv "BLOG_URI"
  title <- getEnv "BLOG_TITLE"
  return Blog {
    blogFolder = folder,
    blogUri = uri,
    blogTitle = title
  }

main :: IO ()
main = do
  blog <- getBlog
  writeBlog blog
