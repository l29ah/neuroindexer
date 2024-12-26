module Main where

-- sorted imports
import Control.Monad (filterM)
import Data.List
import Magic
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.INotify (withINotify, addWatch, WatchDescriptor, Event(..), EventVariety(..))
import System.IO.Temp
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
    -- Get the directory to scan as a command-line argument
    args <- getArgs
    case args of
        [dir] -> do
            files <- filesToProcess dir
            magic <- magicOpen [MagicMimeType]
            magicLoadDefault magic
            setCurrentDirectory dir
            mapM_ (\f -> makeAbsolute f >>= generateDesc magic) files
        _ -> putStrLn "Usage: neuroindexer <directory>"

generateDesc :: Magic -> FilePath -> IO ()
generateDesc magic path = do
    -- Check if the file is an image using libmagic
    isImage <- isImageFile magic path
    if not isImage
        then putStrLn $ path ++ " is not an image file, skipping."
    else do
        putStrLn $ "Generating description for " ++ path
        -- scale the image, using imagemagick, to avoid excessive run time and memory consumption of huge images, meanwhile improving readability of small images for the LLM encoder
        withSystemTempFile "temp.png" $ \tempPath tempHandle -> do
            resizeImage path tempPath

            let descFile = addExtension path ".desc"
                logFile = addExtension path ".desc.log"
                cli = "llama-qwen2vl-cli"
            (exitCode, stdout, stderr) <- readProcessWithExitCode cli (
                "-m" : "./models/Qwen2-VL-7B-Instruct/Qwen2-VL-7B-Instruct-Q5_K_M.gguf" :
                "--mmproj" : "./models/Qwen2-VL-7B-Instruct/mmproj-Qwen2-VL-7B-Instruct-f16.gguf" :
                "-p" : "Describe the important aspects of image, including text on it if there is any." :
                "--temp" : "0.01" :
                "--image" : tempPath :
                []) ""
            writeFile descFile stdout
            appendFile logFile stderr
            case exitCode of
                ExitSuccess -> return ()
                ExitFailure code -> putStrLn $ cli ++ " failed with exit code: " ++ show code

resizeImage :: FilePath -> FilePath -> IO ()
resizeImage inFile outFile = do
    -- 800kpix produces about 1000 qwen2vl tokens
    let targetPixels = "800000@"
    (exitCode, _, _) <- readProcessWithExitCode "magick" [inFile, "-resize", targetPixels, outFile] ""
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> putStrLn $ "magick failed with exit code: " ++ show code

isImageFile :: Magic -> FilePath -> IO Bool
isImageFile magic path = do
    typ <- magicFile magic path
    return $ "image/" `isPrefixOf` typ

-- Produces a lazy list of yet-unscanned files in a directory
filesToProcess :: FilePath -> IO [String]
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
