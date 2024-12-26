module Main where

import System.Environment (getArgs)
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad (filterM)
import System.INotify (withINotify, addWatch, WatchDescriptor, Event(..), EventVariety(..))
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
    -- Get the directory to scan as a command-line argument
    args <- getArgs
    case args of
        [dir] -> do
            files <- filesToProcess dir
            setCurrentDirectory dir
            mapM_ (\f -> makeAbsolute f >>= generateDesc) files
        _ -> putStrLn "Usage: program <directory>"

generateDesc :: String -> IO ()
generateDesc path = do
    putStrLn $ "Generating description for " ++ path
    -- TODO reduce the input image resolution to <1 megapixel first to avoid excessive run time and memory consumption
    let descFile = addExtension path ".desc"
        logFile = addExtension path ".desc.log"
    (exitCode, stdout, stderr) <- readProcessWithExitCode "llama-qwen2vl-cli" (
        "-m" : "./models/Qwen2-VL-7B-Instruct/Qwen2-VL-7B-Instruct-Q5_K_M.gguf" :
        "--mmproj" : "./models/Qwen2-VL-7B-Instruct/mmproj-Qwen2-VL-7B-Instruct-f16.gguf" :
        "-p" : "Describe the important aspects of image, including text on it if there is any." :
        "--temp" : "0.01" :
        "--image" : path :
        []) ""
    writeFile descFile stdout
    appendFile logFile stderr
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> putStrLn $ "generate_desc failed with exit code: " ++ show code

-- Produces a lazy list of yet-unscanned files in a directory
filesToProcess :: String -> IO [String]
filesToProcess dir = do
    -- List all files in the directory
    allFiles <- listDirectory dir
    
    -- Filter out directories and keep only files
    files <- filterM (\f -> doesFileExist (dir </> f)) allFiles
    
    -- Helper function to check if a file needs processing
    let needsProcessing file = do
            let descFile = addExtension file ".desc"
            descExists <- doesFileExist (dir </> descFile)
            if not descExists
                then return True
                else do
                    fileTime <- getModificationTime (dir </> file)
                    descTime <- getModificationTime (dir </> descFile)
                    return $ fileTime > descTime

    -- Filter files that need processing
    toProcess <- filterM needsProcessing files
    
    -- Here we would typically set up inotify for real-time monitoring, but for simplicity:
    -- We'll simulate this by returning the list of files to process now
    return toProcess

    -- TODO: Implement real-time monitoring with inotify
    -- This would involve:
    -- 1. Setting up an INotify watch on the directory
    -- 2. Handling events like file creation, modification, or deletion
    -- 3. Rechecking files when such events occur
    -- Here's a basic outline of how you might start:

    -- withINotify $ \inotify -> do
    --     wd <- addWatch inotify [Create,Delete,Modify] dir $ \event -> do
    --         case event of
    --             Created _ _ -> do
    --                 -- Check if the new file needs a .desc file
    --                 ...
    --             Modified _ _ -> do
    --                 -- Check if the modified file needs its .desc file updated
    --                 ...
    --             Deleted _ _ -> do
    --                 -- Handle file deletion if necessary
    --                 ...
    --     -- Keep the program running to listen for events
    --     forever $ threadDelay 1000000
