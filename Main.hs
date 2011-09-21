module Main where
import Moody
import System (getArgs)
import Data.Maybe
import Data.List

main :: IO ()
main = do
        srcName <- getArgs >>= return.head
        dstName <- return (reverse (fromMaybe (reverse srcName) $ stripPrefix "y." (reverse srcName)) ++ ".hs")
        src <- readFile srcName
        dst <- return$ run src
        writeFile dstName dst

