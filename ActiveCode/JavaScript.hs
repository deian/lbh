module ActiveCode.JavaScript (js) where

import           ActiveCode.Utils

import qualified Data.ByteString.Lazy.Char8 as L8

import           Control.Monad

import           System.Process
import           System.Exit

js :: L8.ByteString -> IO ()
js src = do
  (code,out) <- readCommand "js" [] (L8.unpack src)
  putStr out
  exitWith code
