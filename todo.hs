import System.Environment
import Data.List
import System.Directory
import System.IO

dispatch :: [(String, ([String] -> IO ()))]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] (lines contents)
    putStr (unlines numberedTasks)

add :: [String] -> IO ()
add [fileName, task] = appendFile fileName (task ++ "\n")

remove :: [String] -> IO ()
remove [fileName, numberStr] = do
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

