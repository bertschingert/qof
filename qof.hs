import Control.Monad
import Data.Maybe
import System.Directory
import Text.Read
import System.Environment
import System.Posix.Files

-- The representation of a running process.
data Process  = Process { pid :: Int
                        , comm :: String
                        }

-- An open file may be the process's CWD, its text, or an entry in the open file table.
data OpenFileType = Fd Int | Cwd | Txt

instance Show OpenFileType where
    show (Fd fd) = show fd
    show Cwd = "CWD"
    show Txt = "TXT"

-- The representation of an open file.
data File = File { name :: String
                 , ty :: OpenFileType
                 , proc :: Process
                 }

-- Produce a string of spaces up to `width` wide given that
-- `used` columns are already taken.
pad :: Int -> Int -> String
pad width used = let n = max 1 (width - used) in
    replicate n ' '

-- Add padding on the right of a string to ensure it uses at least `n` columns.
padded :: String -> Int -> String
padded thing n = let thing_len = length thing in
        thing ++ (pad n thing_len)

instance Show File where
    show (File name fd (Process pid comm)) = padded comm 8 ++
                                        padded (show pid) 10 ++
                                        padded (show fd) 4 ++
                                        name

-- Filter out dirents under "/proc/" that are not PIDs.
onlyNumbers :: [FilePath] -> [Int]
onlyNumbers ents  = catMaybes $ map (\ent -> readMaybe ent :: Maybe Int) ents

trimNl :: String -> String
trimNl s = reverse . dropWhile (== '\n') . reverse $ s

-- Path to the file named `name` for the given `pid`
procPath :: Int -> String -> String
procPath pid name = "/proc/" ++ show pid ++ "/" ++ name

-- Print out a line for a single fd of the process.
doOneFile :: Process -> Int -> IO ()
doOneFile proc fd = do
    name <- readSymbolicLink $ procPath (pid proc) "fd/" ++ show fd
    putStrLn . show $ File name (Fd fd) proc

-- Print out a line for the processe's current working directory.
doCwd :: Process -> IO ()
doCwd proc = do
    cwd <- readSymbolicLink $ procPath (pid proc) "cwd"
    putStrLn . show $ File cwd Cwd proc

-- Print out lines for each open file of the process.
doFiles :: Process -> IO ()
doFiles proc = do
    doCwd proc
    fds <- listDirectory $ procPath (pid proc) "fd"
    mapM_ (doOneFile proc . read) fds

doOnePid :: Int -> IO ()
doOnePid pid = do
    comm <- readFile $ procPath pid "comm"
    let p = Process pid (trimNl comm)
    doFiles p

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
