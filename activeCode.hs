module Main (main) where

import           Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L8
import           System.Environment
import           System.Exit
import           Control.Monad

import           ActiveCode.Languages
import           ActiveCode.Haskell

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ putStr "No language specified" >> exitFailure
  let mlangAct = lookup (args!!0) langMap
  unless (isJust mlangAct) $ do
    -- Don't know how to handle this
    putStr ("Sorry, " ++ args!!0 ++ " is not yet supported") 
    exitFailure
  src <- L8.getContents
  fromJust mlangAct $ src
