{-# LANGUAGE OverloadedStrings #-}
module LBH.ActiveCode ( Code(..)
                      , Result(..)
                      , execCode
                      , extractActieCodeBlocks
                      , activeCodeToInactiveBlocks
                      ) where

import           Prelude hiding (id)

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Aeson hiding (Result)
import           Data.List (stripPrefix, concat, intercalate)
import           Data.Maybe
import           Data.Monoid

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
import           Text.Blaze.Html5 (textarea, dataAttribute, toHtml, toValue, (!))
import           Text.Blaze.Html5.Attributes (id, name, class_)


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
  let _id = "raw-active-code-" ++ (maybe (show x) ("named-"++) mName)
  return $ [(RawBlock "html" $ renderHtml $ do
              textarea ! id (toValue _id)
                       ! name (toValue _id)
                       ! class_ "raw-active-code"
                       ! dataAttribute "lang" (toValue lang)
                       ! (if isJust mDeps
                            then dataAttribute "deps" (toValue (fromJust mDeps))
                            else mempty)
                       ! (if isJust mNoExec
                            then dataAttribute "noexec" "true"
                            else mempty)
                       $ toHtml blk')
            ]
 where isActiveCode = isJust . getActiveLang
       getActiveLang (_,(x:_),_) = stripPrefix "active-" x
       getActiveLang _            = Nothing
       --
       lang = fromJust $ getActiveLang attrs
       -- drop first two lines if they're :name and :requires
       blk' = let d = (if isJust mName then 1 else 0) + 
                      (if isJust mDeps then 1 else 0)
              in unlines' $ drop d $ lines blk
       --
       mName = microDSL $ \line -> do
         mn <- stripPrefix ":name=" line
         safeHead $ words mn
       --
       mDeps = microDSL $ \line -> do 
         dps  <- stripPrefix ":requires=" line
         return . show . (map ("raw-active-code-named-"++)) . words $ dps
       --
       mNoExec = microDSL $ \line -> do 
         stripPrefix ":noexec" line
       --
       microDSL :: (String -> Maybe String) -> Maybe String
       microDSL act = let ls = lines blk
                          n  = min 3 (length ls)
                      in microDSLdoit act $ take n ls
       --
       microDSLdoit act []     = Nothing
       microDSLdoit act (l:ls) = case act l of
                                  x@(Just _) -> x
                                  _ -> microDSLdoit act ls
       --
       safeIdx xs i = if i < length xs then Just (xs!!i) else Nothing
       --
       safeHead = \x -> safeIdx x 0
       --
       unlines' = intercalate "\n"
       
extractFromBlock b = return [b]

-- | Generate a fresh variable
freshVar :: M Int
freshVar = do
  i <- get
  put (i+1)
  return i

--
--
--

  

-- | Make active code inactive
activeCodeToInactiveBlocks :: Pandoc -> Pandoc
activeCodeToInactiveBlocks (Pandoc i bs) = Pandoc i (map a2i bs)
 where a2i :: Block -> Block
       a2i (CodeBlock attrs blk) =
           let attrs' = case attrs of
                 (x,(lang:y),z) -> (x,(stripPrefix' "active-" lang:y),z)
                 _  -> attrs
           in CodeBlock attrs' blk
       a2i b = b
       stripPrefix' x xs = fromMaybe xs $ stripPrefix x xs
