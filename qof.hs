import Control.Exception
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

-- An open file may be:
--   the process's CWD
--   its text
--   an entry in the open file table
--   an entry representing failure to read some information
data OpenFileType = Fd Int | Fail String | Cwd | Txt

instance Show OpenFileType where
    show (Fd fd) = show fd
    show (Fail reason) = reason
    show Cwd = "cwd"
    show Txt = "txt"

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
rightPad :: String -> Int -> String
rightPad thing n = let thing_len = length thing in
        thing ++ (pad n thing_len)

-- Add padding on the left and right of a string to ensure it uses at least
-- `before` + `after` columns.
leftPad :: Int -> String -> Int -> String
leftPad before thing after = let thing_len = length thing in
        (pad before thing_len) ++ thing ++ (replicate after ' ')

instance Show File where
    show (File name fd (Process pid comm)) = rightPad comm 8 ++
                                        leftPad 10 (show pid) 1 ++
                                        leftPad 5 (show fd) 2 ++
                                        name

-- Filter out dirents under "/proc/" that are not PIDs.
onlyNumbers :: [FilePath] -> [Int]
onlyNumbers ents  = catMaybes $ map (\ent -> readMaybe ent :: Maybe Int) ents

trimNl :: String -> String
trimNl s = reverse . dropWhile (== '\n') . reverse $ s

-- Path to the file named `name` for the given `pid`
procPath :: Int -> String -> String
procPath pid name = "/proc/" ++ show pid ++ "/" ++ name

-- Given a `path` and the result of trying to read that path, construct a
-- string with either its actual data or an indication that an error occurred.
nameOrError :: String -> Either IOError String -> String
nameOrError path (Left e) = path ++ " (Could not read file)"
nameOrError _ (Right val) = val

-- Print out a line for a single fd of the process.
doOneFile :: Process -> Int -> IO ()
doOneFile proc fd = do
    let path = procPath (pid proc) "fd/" ++ show fd
    name_or_error <- try $ do readSymbolicLink path
    let name = nameOrError path name_or_error
    print $ File name (Fd fd) proc

-- Print out a line for the processe's current working directory.
doCwd :: Process -> IO ()
doCwd proc = do
    let path = procPath (pid proc) "cwd"
    cwd_or_error <- try $ readSymbolicLink path
    let cwd = nameOrError path cwd_or_error
    print $ File cwd Cwd proc

-- Print out lines for each open file of the process.
doFiles :: Process -> IO ()
doFiles proc = do
    doCwd proc
    let fd_path = procPath (pid proc) "fd"
    fds_or_error <- try $ do listDirectory fd_path
    case (fds_or_error :: Either IOError [FilePath]) of
                    Left e -> print $
                        File (fd_path ++ " (Could not read directory)") (Fail "NOFD") proc
                    Right fds -> mapM_ (doOneFile proc . read) fds

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
