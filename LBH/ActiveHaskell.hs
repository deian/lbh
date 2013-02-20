{-# LANGUAGE OverloadedStrings #-}
module LBH.ActiveHaskell (Code(..), Result(..), execCode) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Aeson hiding (Result)

import           Control.Applicative
import           Control.Monad


import           LIO
import           LIO.DCLabel
import           System.Exit

-- TODO: Use cjail 
import System.Process
import LIO.TCB (ioTCB)

-- | Code from clients
data Code = Code { codeId     :: String
                 , codeSource :: L8.ByteString
                 } deriving (Eq, Show)

instance FromJSON Code where
  parseJSON (Object v) = Code <$> v .: "id" <*> v .: "source"
  parseJSON _          = mzero

-- | Result types, currently no interactive 
data Result = Result { resultId    :: String
                     , resultCode  :: Int
                     , resultValue :: L8.ByteString
                     } deriving (Eq, Show)

instance ToJSON Result where
  toJSON (Result id c r) = object [ "id"     .= id
                                  , "code"   .= c
                                  , "result" .= r ]

execCode :: Code -> DC Result
execCode c = do
  (code,out,_) <- ioTCB $ readProcessWithExitCode "activeHaskell" [] (L8.unpack $ codeSource c)
  let rc = if code == ExitSuccess then 0 else -1
  return $ Result { resultId    = codeId c
                  , resultCode  = rc
                  , resultValue = L8.pack $ out }