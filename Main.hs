{-# LANGUAGE LambdaCase,TupleSections #-}
module Main where

import System.Directory
import System.Posix.Files
import System.FilePath (combine,(</>))
import System.Environment ( getArgs )
import System.Posix.Process ( getProcessID)
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Console.ANSI
import Data.Maybe
import Data.String
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import System.IO
import Data.Word

isNoSpecialDir "." = False
isNoSpecialDir ".." = False
isNoSpecialDir _ = True

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

handleIO = flip catchIO

filterJust = map fromJust . filter isJust

align :: Int -> String -> String
align m n = replicate (min m 2 - length n) ' ' ++ n

watch :: Int -> Int -> IO ()
watch interval pid = 
        let fds = "/proc" </> show pid </> "fd"
            names = filter isNoSpecialDir <$> getDirectoryContents fds
            getlink name = handleIO (const $ return Nothing) $ 
                let path = fds </> name in do
                    islink <- isSymbolicLink <$> getSymbolicLinkStatus path
                    target <- if islink 
                                    then readSymbolicLink path
                                    else return "no symlink?!"
                    return $ Just (name,target)
        in do
            names' <- fmap filterJust $ mapM getlink =<< names
            let m = maximum $ map (length . fst) names'
                output (l,t) = putStrLn $ align m l ++ " -> " ++ t
            clearScreen
            mapM output names'
            threadDelay interval
            watch interval pid

numbersOnly :: [String] -> [Int]
numbersOnly = sort . foldl addIfNumber []
    where addIfNumber xs x = case reads x of 
            [(n,"")] -> n:xs
            _ -> xs


getCmdLine :: Int -> IO BS.ByteString
getCmdLine pid = bracket (openBinaryFile ("/proc" </> show pid </> "cmdline") ReadMode) hClose
                    (BS.hGetLine >=> return . BS.map mapNull . BS.init)
    where mapNull :: Word8 -> Word8
          mapNull 0 = fromIntegral $ fromEnum ' '
          mapNull x = x

mapMNonFailing :: (a -> IO b) -> [a] -> IO [b]
mapMNonFailing f = foldr accum (return []) . map f
    where accum a b = catchIO ( (:) <$> a <*> b ) $ const b
    

main = getArgs >>= \args ->
    let (interval,arg) = if (head args) == "-i" 
                                then (read (args !! 1), args !! 2)
                                else (1               , head args)
        watch' = watch $ interval * 10^6
        selectProcess myPid = 
            let 
                allPids = numbersOnly . filter isNoSpecialDir <$> getDirectoryContents "/proc"
                addCmdLine pid = getCmdLine pid >>= return . (pid,)
                pidsWithCmdlines = allPids >>= mapMNonFailing addCmdLine
                myFilter (pid,cmdline) = (fromString arg `BS.isInfixOf` cmdline) && myPid /= pid
            in filter myFilter <$> pidsWithCmdlines >>= \case
                [] -> putStrLn $ "No process matching \""++arg++"\" found."
                filtered -> let numbers = take (length filtered) [1..] 
                                alignnumbers = maximum $ map (length.show) numbers
                                alignpids = maximum $ map (length.show.fst) filtered
                                output (n,(pid,cmdline)) = putStr (align alignnumbers (show n) ++ ") " 
                                                               ++ align alignpids (show pid)
                                                               ++ ": ") >> BSC8.putStrLn cmdline
                            in do mapM output $ zip numbers filtered
                                  reads <$> getLine >>= \case
                                    [(n,"")] -> watch' $ fst $ filtered !! (n - 1)
                                    _        -> selectProcess myPid

    in case reads $ arg of
        [(pid,"")] -> watch' pid
        _          -> fromIntegral <$> getProcessID >>= selectProcess
        
            
    
