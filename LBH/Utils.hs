module LBH.Utils ( lines' ) where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T


-- | /O(n)/ Portably breaks a 'Text' up into a list of 'Text's at line
-- boundaries.
--
-- A line boundary is considered to be either a line feed, a carriage
-- return immediately followed by a line feed, or a carriage return.
-- This accounts for both Unix and Windows line ending conventions,
-- and for the old convention used on Mac OS 9 and earlier.
--
-- This was grabbed from:
--
-- Module      : Data.Text
-- Copyright   : (c) 2009, 2010, 2011, 2012 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--
-- License     : BSD-style
lines' :: Text -> [Text]
lines' ps | T.null ps  = []
          | otherwise = h : case T.uncons t of
                              Nothing -> []
                              Just (c,t')
                                  | c == '\n' -> lines' t'
                                  | c == '\r' -> case T.uncons t' of
                                                   Just ('\n',t'') -> lines' t''
                                                   _               -> lines' t'
    where (h,t)    = T.span notEOL ps
          notEOL c = c /= '\n' && c /= '\r'
{-# INLINE lines' #-}