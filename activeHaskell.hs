module Main (main) where

import qualified Data.ByteString.Char8 as S8
import System.Directory
import System.Process
import System.Exit
import Control.Monad

main = do
  src <- S8.getContents
  let dir = "/tmp/active-haskell"
  createDirectoryIfMissing True dir
  setCurrentDirectory dir
  S8.writeFile "Main.hs" src
  compile "Main"
  run "./Main"


compile :: FilePath -> IO ()
compile fname = do
  (code,out,err) <- readProcessWithExitCode "ghc"
                     ["-XSafe","-o",fname , fname++".hs"] ""
  unless (code == ExitSuccess) $ putStr err >> exitFailure

run :: FilePath -> IO ()
run fname = do
  (code,out,err) <- readProcessWithExitCode fname [] ""
  if code == ExitSuccess
    then putStr out >> exitSuccess
    else putStr err >> exitFailure