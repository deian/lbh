module ActiveCode.Bash (bash) where

import           ActiveCode.Utils

import qualified Data.ByteString.Lazy.Char8 as L8

import           Control.Monad

import           System.Process
import           System.Exit

bash :: L8.ByteString -> IO ()
bash src = do
  (code,out) <- readCommand "bash" [] (L8.unpack src)
  putStr out
  exitWith code
