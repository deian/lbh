module ActiveCode.JavaScript (js) where

import qualified Data.ByteString.Lazy.Char8 as L8

import           Control.Monad

import           System.Process
import           System.Exit

js :: L8.ByteString -> IO ()
js src = do
  (code,out,err) <- readProcessWithExitCode "js" [] (L8.unpack src)
  unless (code == ExitSuccess) $ putStr err >> exitWith code
  putStr out
  exitSuccess