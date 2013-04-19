{-# LANGUAGE OverloadedStrings #-}
module ActiveCode.Haskell (haskell) where

import           ActiveCode.Utils

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Control.Exception
import           Control.Monad

import           System.Posix.Temp
import           System.Process
import           System.Exit
import           System.IO (hClose, hFlush)

haskell :: L8.ByteString -> IO ()
haskell src = do
  hs <- bracket (mkstemps "/tmp/activeHaskell" ".hs")
                (hClose . snd)
                (\(hs,h) -> L8.hPut h src >> return hs)
  (code,out') <- readCommand "runghc" [hs] ""
  let rpl = T.replace (T.pack hs) "<user-input>" 
      out = rpl $ T.pack out'
  T.putStr out
  exitWith code
