{-# LANGUAGE OverloadedStrings #-}
module LBH.Views where

import           Prelude hiding (div, span, head, id)
import           LBH.MP

import           Control.Monad
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid (mempty)
import           Data.Time
import           Hails.Web hiding (body)
import           Hails.HttpServer.Types
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (label, form, span, title, style)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8

import qualified Text.Pandoc.Readers.Markdown as P
import qualified Text.Pandoc.Writers.HTML as P
import qualified Text.Pandoc.Options as P
import qualified Text.Pandoc.Highlighting as P

import Debug.Trace

respondHtml :: Maybe UserName -> Html -> Response
respondHtml muser content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    title "Learn By Hacking"
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
    script ! src "/static/js/jquery.min.js" $ ""
    script ! src "/static/js/bootstrap.min.js" $ ""
  body $ do
     div ! class_ "navbar navbar-fixed-top navbar-inverse" $ do
       div ! class_ "navbar-inner" $ do
         div ! class_ "container" $ do
           a ! href "/" ! class_ "brand" $ "Learn By Hacking" 
           ul ! class_ "nav pull-right" $ case muser of 
            Nothing -> do
              li $ a ! href "/login" $ do
               span ! class_ "icon-user icon-white" $ ""
               " Login"
            Just u -> li ! class_ "pull-right" $ a ! href "#" $ do
                        img ! src "https://secure.gravatar.com/avatar/?s=14"
                        " "
                        toHtml u
     div ! class_ "container" $ content

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" 
                      ! type_ "text/css" ! href (toValue uri)

--
-- Posts
--

newPost :: UserName -> Html
newPost usr = do
  stylesheet "/static/css/application/posts.css"
  h1 $ "Create a new post"
  div $ do
    form ! action "/posts" ! method "POST" ! id "newPost" $ do
      div $ do
        input ! type_ "hidden" ! name "owner"
              ! value (toValue usr)
      div $ do
        label ! for "title" $ "Title:"
        input ! class_ "span12" ! type_ "text"
              ! name "title" ! id "title"
      div $ do
        label ! for "description" $ "Description:"
        input ! class_ "span12" ! type_ "text"
              ! name "description" ! id "description"
      div $ do
        label ! for "body" $ "Body:"
        textarea ! class_ "span12"
                 ! name "body" ! id "body" $ ""
      div ! class_ "btn-group" $ do
        input ! type_ "submit" ! class_ "btn" ! value "Create"

editPost :: UserName -> Post -> Html
editPost usr post = do
  stylesheet "/static/css/application/posts.css"
  script ! src "/static/js/application/posts.js" $ ""
  h1 $ "Edit post"
  div ! id "post-edit" $ do
    form ! action "/posts" ! method "POST" ! id "editPost" $ do
      div $ do
        input ! type_ "hidden" ! name "_method" ! value "PUT"
        input ! type_ "hidden" ! name "_id"
              ! value (toValue $ show $ getPostId post)
      div $ do
        label ! for "title" $ "Title:"
        input ! class_ "span12" ! type_ "text"
              ! name "title" ! id "title"
              ! value (toValue $ postTitle post)
      div $ do
        label ! for "description" $ "Description:"
        input ! class_ "span12" ! type_ "text"
              ! name "description" ! id "description"
              ! value (toValue $ postDescription post)
      div $ do
        label ! for "body" $ "Body:"
        textarea ! class_ "span12"
                 ! name "body" ! id "body" $ toHtml $ postBody post
      div ! class_ "btn-group" $ do
        a ! class_ "btn" ! id "post-save-btn" $ do
          i ! class_ "icon-download-alt" $ ""
          " Save"
        unless (postIsPublic post) $ do
          a ! class_ "btn"
            ! id "post-make-public-btn" $ do
            i ! class_ "icon-share" $ ""
            " Make Public"
        a ! class_ "btn"
          ! id "post-preview-btn"
          ! dataAttribute "toggle" "button" $ do
            i ! class_ "icon-eye-open" $ ""
            " Preview"
  div ! id "post-preview" ! A.style "display: none" $ do
    ul ! class_ "breadcrumb" $ do
       li $ "Preview..."
       li ! class_ "pull-right" $ do
         a ! href "#"
           ! id "refresh-post-preview-btn"
           ! A.title "Refresh" $ i ! class_ "icon-refresh" $ ""
    div ! id "post-preview-body"
        ! dataAttribute "src" (toValue $ "/posts/" ++ (show $ getPostId post))
        $ ""

showPost :: Maybe UserName -> Post -> Html
showPost muser post = do
  h1 $ toHtml $ postTitle post
  -- Include post header
  div ! id "post-header" $ do
    ul ! class_ "inline" $ do
      li $ do
        i ! class_ "icon-time" $ ""
        toHtml $ " " ++ showDate (postDate post)
      li $ "|"
      li $ a ! href (toValue $ "/users/" `T.append` postOwner post) $ do
        i ! class_ "icon-user" $ ""
        " "
        toHtml $ postOwner post
      when (muser == Just (postOwner post)) $ do
        li $ "|"
        li $ a ! href (toValue $ "/posts/" ++ (show $ getPostId post)
                              ++ "/edit") $ do
          i ! class_ "icon-edit" $ ""
          " edit"
  hr
  div ! id "post-body" $ do
    -- Include kate syntax highlighting css
    style $ toHtml $ P.styleToCss P.kate
    -- Include post body
    div $ do
      P.writeHtml wopts $
       P.readMarkdown ropts (T.unpack . crlf2lf $ postBody post)
     where ropts = P.def { P.readerExtensions     = P.githubMarkdownExtensions }
           wopts = P.def { P.writerHighlight      = True
                         , P.writerHighlightStyle = P.kate
                         , P.writerHtml5          = True
                         , P.writerExtensions     = P.githubMarkdownExtensions }

indexPosts :: Maybe UserName -> [Post] -> Html
indexPosts musr ps = do
  stylesheet "/static/css/application/posts.css"
  h1 $ "Posts"
  when (isJust musr) $ do
    a ! class_ "btn btn-primary" ! href "new" $ do
      i ! class_ "icon-plus icon-white" $ ""
      " New Post"
  hr
  div $ do
    ul ! class_ "media-list " ! id "index-posts" $ do
      forM_ ps $ \post -> do
        let postUrl = "/posts/" ++ show (getPostId post)
        li ! class_ "media"
           ! onclick (toValue $ "location.href=\'" ++ postUrl ++ "\'") $ do
          img ! class_ "pull-left media-object"
              ! src "https://secure.gravatar.com/avatar/?s=48"
          div ! class_ "media-body" $ do
              h4 ! class_ "media-heading" $ do
                a ! href (toValue postUrl) $ toHtml (postTitle post)
              div ! class_ "pull-right" $ do
                ul ! class_ "inline" $ do
                   li $ toHtml $ showDate (postDate post)
                   li $ "|"
                   li $ a ! href (toValue $ "/users/" `T.append` postOwner post)
                          $ toHtml $ postOwner post
                   unless (postIsPublic post) $ do
                   li $ "|"
                   li $ a ! href "#"
                          ! dataAttribute "toggle" "tooltip"
                          ! A.title (fst $ privInfo post) $
                            i ! class_ (snd $ privInfo post) $ ""
              div $ toHtml $ postDescription post
  where privInfo p = if postIsPublic p
                       then ("Public post", "icon-globe")
                       else ("Private post", "icon-lock")
                                 

--
-- Helper functions
--



-- | Show time
showDate :: UTCTime -> String
showDate = showGregorian . utctDay
                    
-- | Replace all CR-LFs to LFs, so the Markdown parser works fine
crlf2lf :: Text -> Text
crlf2lf = T.unlines . lines'
                    
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