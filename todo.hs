import System.Environment
import Data.List
import System.Directory
import System.IO
import System.IO.Error
import Control.Exception

dispatch :: [(String, ([String] -> IO ()))]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, task] = appendFile fileName (task ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    catch (_view [fileName]) (readFileExceptionHandler fileName)

_view :: [String] -> IO ()
_view [fileName] = do
    contents <- readFile fileName
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] (lines contents)
    putStr (unlines numberedTasks)

remove :: [String] -> IO ()
remove [fileName, numberStr] = do
    catch (_remove [fileName, numberStr]) (readFileExceptionHandler fileName)

_remove :: [String] -> IO ()
_remove [fileName, numberStr] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "todo"
    contents <- hGetContents handle
    let number = read numberStr
        tasks = lines contents
        newTasks = delete (tasks !! number) tasks
    hPutStr tempHandle (unlines newTasks)
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

readFileExceptionHandler :: String -> IOError -> IO ()  
readFileExceptionHandler fileName e  
    | isDoesNotExistError e = putStrLn ("The file " ++ fileName ++ " doesn't exist!")
    | otherwise = ioError e  

