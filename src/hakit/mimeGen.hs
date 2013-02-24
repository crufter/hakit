-- Generates the bulk of extMime.hs (into mime.hs) file from the file mimes.txt, which is copied from
-- http://svn.apache.org/repos/asf/httpd/httpd/branches/2.0.x/docs/conf/mime.types

import qualified Data.List.Split as Spl
import qualified Data.List as L

generate :: String -> IO ()
generate folderPath = do
    let source  = folderPath ++ "mimes.txt"
        target  = folderPath ++ "mimeExtracted.hs"
        f x     = if length x == 0
            then False
            else x!!0 /= '#'
    src <- readFile source
    let filteredLines = filter f $ lines src
        toPair x = let parts = Spl.wordsBy (== '\t') x in
            if length parts > 1
                then map (\x -> (x, parts!!0)) $ Spl.splitOn " " (parts!!1)
                else error $ "Something is wrong: " ++ x
        pairs :: [(String, String)]
        pairs = concat $ map toPair filteredLines
    writeFile target $ unlines $ map (\(k, v) -> "\t\t(" ++ show k ++ "," ++ (L.replicate (16 - length k) ' ') ++ show v ++ ")," ) pairs