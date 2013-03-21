{-# LANGUAGE OverloadedStrings #-}
module ActiveCode.C (c,cpp) where

import qualified Data.ByteString.Lazy.Char8 as L8

import           Control.Exception
import           Control.Monad

import           System.FilePath
import           System.Posix.Temp
import           System.Process
import           System.Exit
import           System.IO (hClose)

import qualified Data.Text as T
import qualified Data.Text.IO as T

c :: L8.ByteString -> IO ()
c = clang "clang" ".c"

cpp :: L8.ByteString -> IO ()
cpp = clang "clang++" ".cpp"

clang :: String -> String -> L8.ByteString -> IO ()
clang cc ext src = do
  fp <- bracket (mkstemps "/tmp/activeC" ext)
                (hClose . snd)
                (\(f,h) -> L8.hPut h src >> return (dropExtension f))
  let rpl = T.replace (T.pack $ fp++ext) "<user-input>" 
  -- compile
  (ccode,_,cerr') <- readProcessWithExitCode cc ["-o",fp,fp++ext] ""
  let cerr = rpl $ T.pack cerr'
  unless (ccode == ExitSuccess) $ T.putStr cerr >> exitWith ccode
  -- run
  (code,out',err') <- readProcessWithExitCode fp [] ""
  let out = rpl $ T.pack out'
      err = rpl $ T.pack err'
  unless (code == ExitSuccess) $ T.putStr err >> exitWith code
  T.putStr out
  exitSuccess
