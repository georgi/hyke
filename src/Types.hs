module Types where

import Data.Time.Format
import Data.Time.Calendar
import Data.List
import Data.List.Split
import System.Locale
import Text.Sundown.Html.String as S

type Page = (Int, Int)

data Blog = Blog {
  blogFolder,
  blogUri,
  blogTitle :: String
};

data Post = Post {
  postYear :: Integer,
  postMonth :: Int,
  postDay :: Int,
  postFile,
  postText:: String
};

pubDate :: Day -> String
pubDate = formatTime defaultTimeLocale "%d %b %y 00:00"

postTitle :: Post -> String
postTitle post = head $ lines $ postText post

postName :: Post -> String
postName post = head $ splitOn "." $ postFile post

postLink :: Post -> String
postLink post = postName post ++ ".html"

postDate :: Post -> String
postDate post = pubDate date where
  date = fromGregorian (postYear post) (postMonth post) (postDay post)

postBody :: Post -> String
postBody post = S.renderHtml s allExtensions noHtmlModes True Nothing
  where s = intercalate "\n" $ drop 3 $ lines $ postText post
