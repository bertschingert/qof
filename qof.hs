import Control.Monad
import Data.Maybe
import System.Directory
import Text.Read
import System.Environment

-- The representation of a running process.
data Process  = Process { pid :: Int
                        , comm :: String
                        }

-- Produce a string of spaces up to `width` wide given that
-- `used` columns are already taken.
pad :: Int -> Int -> String
pad width used = let n = max 1 (width - used) in
    replicate n ' '


-- Show an object with padding on the right to ensure it uses
-- at least `n` columns.
showPadded :: Show a => a -> Int -> String
showPadded thing n = let thing_len = length $ show thing in
        show thing ++ (pad n thing_len)

instance Show Process where
    show (Process pid comm) = showPadded pid 15 ++ comm

onlyNumbers :: [FilePath] -> [Int]
onlyNumbers ents  = catMaybes $ map (\ent -> readMaybe ent :: Maybe Int) ents

trimNl :: String -> String
trimNl s = reverse . dropWhile (== '\n') . reverse $ s

doOnePid :: Int -> IO ()
doOnePid pid = do
    let f = "/proc/" ++ show pid ++ "/comm"
    comm <- readFile f
    let p = Process pid (trimNl comm)
    print p

-- Given nonempty `args`, converts the list of arguments into a list of PIDs,
-- or else returns a list of all the PIDs in /proc.
getPidList :: [String] -> IO [Int]
getPidList [] = do
    dirs <- listDirectory "/proc"
    return (onlyNumbers dirs)
getPidList args = return (onlyNumbers args)

main = do
    args <- getArgs
    pids <- getPidList args
    mapM_ doOnePid pids
