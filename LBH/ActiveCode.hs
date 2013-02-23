{-# LANGUAGE OverloadedStrings #-}
module LBH.ActiveCode ( Code(..)
                      , Result(..)
                      , execCode
                      , extractActieCodeBlocks
                      ) where

import           Prelude hiding (div, id)

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Aeson hiding (Result)
import           Data.List (stripPrefix)
import           Data.Maybe

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State


import           LIO
import           LIO.DCLabel
import           LIO.CJail
import           System.Exit

import           Text.Pandoc hiding (Code)

import           ActiveCode.Languages (languages)

import           Text.Blaze.Html.Renderer.String
import           Text.Blaze.Html5 (div, dataAttribute, toHtml, toValue, (!))
import           Text.Blaze.Html5.Attributes (id, class_)


-- | Code from clients
data Code = Code { codeId       :: String
                 , codeLanguage :: String
                 , codeSource   :: L8.ByteString
                 } deriving (Eq, Show)

instance FromJSON Code where
  parseJSON (Object v) = Code <$> v .: "id" <*> v .: "lang"
                                            <*> v .: "source"
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
  (code,out,_) <- inCJail $
     readProcessWithExitCode "activeCode" [lang] (source)
  let rc = if code == ExitSuccess then 0 else -1
  return $ Result { resultId    = codeId c
                  , resultCode  = rc
                  , resultValue = L8.pack $ out }
     where lang   = head . lines $ codeLanguage c
           source = L8.unpack $ codeSource c

--
--
--

type M = State Int

extractActieCodeBlocks :: Pandoc -> Pandoc
extractActieCodeBlocks (Pandoc i bs) = (flip evalState) 0 $ do
  bs' <- concat `liftM` forM bs extractFromBlock
  return $ Pandoc i bs'

-- | Currently only handle top-level blocks
extractFromBlock :: Block -> M [Block]
extractFromBlock (CodeBlock attrs blk) | isActiveCode attrs
                                       && lang `elem` languages = do
  x <- freshVar
  let _id = "active-" ++ lang ++ "-" ++ show x
  return $ [(CodeBlock (_id , [lang, "active-code", "active-"++lang],[]) blk)
           ,(RawBlock "html" $ renderHtml $ do
              div ! id (toValue $ "raw-" ++ _id)
                  ! class_ "raw-active-code"
                  ! dataAttribute "lang" (toValue lang) $ toHtml blk)
            ]
 where isActiveCode = isJust . getActiveLang
       getActiveLang (_,(x:_),_) = stripPrefix "active-" x
       getActiveLang _            = Nothing
       lang = fromJust $ getActiveLang attrs
extractFromBlock b = return [b]

-- | Generate a fresh variable
freshVar :: M Int
freshVar = do
  i <- get
  put (i+1)
  return i