import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.List.Split
import Text.HandsomeSoup
import Text.Regex.Posix
import Text.XML.HXT.Core


main :: IO ()
main = do
    threadUrls <- fetchThreadUrls
    mapM_ downloadThread threadUrls


fetchThreadUrls :: IO [String]
fetchThreadUrls = do
    doc <- runX (fromUrl catalogUrl >>> css "script" >>> deep getText)
    let threadIds = extractThreadIds $ concat doc
    let threadUrls = constructThreadUrls threadIds
    return threadUrls


catalogUrl :: String
catalogUrl = "http://boards.4chan.org/wg/catalog"


extractThreadIds :: String -> [String]
extractThreadIds s = let getId      = init . tail . head . splitOn ":"
                         matchIds x = getAllTextMatches (x =~ "\"[0-9]+\":\\{\"date\"" :: AllTextMatches [] String)
                     in map getId $ matchIds s


constructThreadUrls :: [String] -> [String]
constructThreadUrls = map (threadUrl ++)


threadUrl :: String
threadUrl = "http://boards.4chan.org/wg/res/"


downloadThread :: String -> IO()
downloadThread url = do
    images <- runX $ fromUrl url >>> css "a" ! "href" >>> isA (isPrefixOf "//i.4cdn.org/")
    mapM_ (download . fixUrl) images


fixUrl :: String -> String
fixUrl = (++) "http:"


download :: String -> IO ()
download url = do
    content <- runMaybeT $ openUrl url
    case content of
        Nothing -> putStrLn $ "bad url: " ++ url
        Just _content -> do
            let name = last . splitOn "/" $ url
            B.writeFile name (B.pack _content)
