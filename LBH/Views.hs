{-# LANGUAGE OverloadedStrings #-}
module LBH.Views where

import           Prelude hiding (div, span, head, id)
import           LBH.MP
import           LBH.Utils

import           Control.Monad
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Monoid (mempty)
import           Data.Time
import qualified Data.Digest.Pure.MD5 as MD5
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

respondHtml :: Maybe User -> Html -> Response
respondHtml muser content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    title "Learn By Hacking"
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
    script ! src "/static/js/jquery.min.js" $ ""
    script ! src "/static/js/bootstrap.min.js" $ ""
  body $ do
     div ! class_ "navbar navbar-fixed-top navbar-inverse"
         ! id "page-nav" $ do
       div ! class_ "navbar-inner" $ do
         div ! class_ "container" $ do
           a ! href "/" ! class_ "brand" $ "Learn By Hacking" 
           ul ! class_ "nav pull-right" $ case muser of 
            Nothing -> do
              li $ a ! href "/login" $ do
               span ! class_ "icon-user icon-white" $ ""
               " Login"
            Just u -> li ! class_ "pull-right" $
              a ! href (toValue $ "/users/" `T.append` userId u) $ do
                img ! src (toValue $ T.concat [
                  "https://secure.gravatar.com/avatar/"
                  , md5 (userEmail u), "?s=16"])
                " "
                toHtml (userId u)
     div ! class_ "container" $ content

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" 
                      ! type_ "text/css" ! href (toValue uri)

--
-- Posts
--

newPost :: User -> Html
newPost usr = do
  stylesheet "/static/css/bootstrap-tagmanager.css"
  stylesheet "/static/css/application/posts.css"
  script ! src "/static/js/bootstrap-tagmanager.js" $ ""
  script ! src "/static/js/application/posts.js" $ ""
  --
  h1 $ "Create a new post"
  div $ do
    form ! action "/posts" ! method "POST" ! id "newPost" $ do
      div $ do
        input ! type_ "hidden" ! name "owner"
              ! value (toValue $ userId usr)
      div $ do
        label ! for "title" $ "Title:"
        input ! class_ "span12" ! type_ "text"
              ! name "title" ! id "title"
              ! placeholder "Writing Haskell for Fun and Profit"
      div $ do
        label ! for "description" $ "Description:"
        input ! class_ "span12" ! type_ "text"
              ! name "description" ! id "description"
      div $ do
        label ! for "body" $ "Body:"
        textarea ! class_ "span12"
                 ! name "body" ! id "body" $ ""
      div $ do
        label ! for "tags" $ do
          "Tags ("
          a ! href "#"
            ! dataAttribute "toggle" "tooltip"
            ! A.title "All tags are public"
            $ "?"
          "):"
        input ! class_ "tagManager" ! type_ "text"
              ! name "tagsAggr"
              ! placeholder "haskell, fun, profit, ..."
      div ! class_ "btn-group" $ do
        input ! type_ "submit" ! class_ "btn" ! value "Create"

editPost :: Post -> Html
editPost post = do
  stylesheet "/static/css/bootstrap-tagmanager.css"
  stylesheet "/static/css/application/posts.css"
  script ! src "/static/js/bootstrap-tagmanager.js" $ ""
  script ! src "/static/js/application/posts.js" $ ""
  --
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
      div $ do
        label ! for "tags" $ "Tags:"
        input ! class_ "tagManager" ! type_ "text"
              ! name "tagsAggr"
              ! if (null $ postTags post)
                 then placeholder "haskell, fun, profit, ..."
                 else mempty
        input ! type_ "hidden"
              ! id "prefilled-tagsAggr"
              ! value (toValue $ T.intercalate "," $ postTags post)
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
    iframe ! id "post-preview-body"
           ! src (toValue $ "/posts/" ++ (show $ getPostId post))
           $ ""

showPost :: Maybe User -> Post -> Html
showPost muser post = do
  stylesheet "/static/css/application/posts.css"
  script ! src "/static/js/application/posts.js" $ ""
  -- Include post header
  div ! class_ "page-header" ! id "post-header" $ do
    h1 $ toHtml $ postTitle post
    ul ! class_ "inline" $ do
      li $ do
        i ! class_ "icon-time" $ ""
        toHtml $ " " ++ showDate (postDate post)
      li $ "|"
      li $ a ! href (toValue $ "/users/" `T.append` postOwner post) $ do
        i ! class_ "icon-user" $ ""
        " "
        toHtml $ postOwner post
      when (userId `liftM` muser == Just (postOwner post)) $ do
        li $ "|"
        li $ a ! href (toValue $ "/posts/" ++ (show $ getPostId post)
                              ++ "/edit") $ do
          i ! class_ "icon-edit" $ ""
          " edit"
      unless (null $ postTags post) $ ul ! class_ "inline pull-right" $ do
        li $ i ! class_ "icon-tags" $ ""
        forM_ (zip [1..10] (postTags post)) $ \(_,t) -> do
          li ! class_ "small" $ do
             a ! href (toValue $ "/tags/" `T.append` t) $ toHtml t
          li $ ""
  div ! id "post-body" $ do
    -- Include kate syntax highlighting css
    style $ toHtml $ P.styleToCss P.kate
    -- Include post body
    div $ do
      P.writeHtml wopts $
       let md = P.readMarkdown ropts (T.unpack . crlf2lf $ postBody post)
       in extractHaskellCodeBlocks md
     where ropts = P.def { P.readerExtensions     = P.githubMarkdownExtensions }
           wopts = P.def { P.writerHighlight      = True
                         , P.writerHighlightStyle = P.kate
                         , P.writerHtml5          = True
                         , P.writerExtensions     = P.githubMarkdownExtensions }

indexPosts :: String -> Maybe User -> [(User,Post)] -> Html
indexPosts idxTitle musr ups = do
  stylesheet "/static/css/application/posts.css"
  script ! src "/static/js/application/posts.js" $ ""
  div ! class_ "page-header" $ do
    h1 $ toHtml $ idxTitle
    when (isJust musr) $ do
      a ! class_ "btn btn-primary" ! href "/posts/new" $ do
        i ! class_ "icon-plus icon-white" $ ""
        " New Post"
  div $ if null ups
    then p $ "Sorry, no posts... :-("
    else ul ! class_ "media-list " ! id "index-posts" $ do
           forM_ ups $ \(user,post) -> do
             let postUrl = "/posts/" ++ show (getPostId post)
             li ! class_ "media"
                ! onclick (toValue $ "location.href=\'" ++ postUrl ++ "\'") $ do
               img ! class_ "pull-left media-object"
                   ! src (toValue $ T.concat
                              [ "https://secure.gravatar.com/avatar/"
                              , md5 (userEmail user), "?s=48"])
               div ! class_ "media-body" $ do
                   h4 ! class_ "media-heading" $ do
                     a ! href (toValue postUrl) $ toHtml (postTitle post)
                   div ! class_ "pull-right" $ do
                     ul ! class_ "inline" $ do
                        li $ toHtml $ showDate (postDate post)
                        li $ "|"
                        li $ a ! href (toValue $ "/users/" `T.append`
                                                 postOwner post)
                               $ toHtml $ postOwner post
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
-- Users
--

indexUsers :: Maybe User -> [User] -> Html
indexUsers musr ps = do
  stylesheet "/static/css/application/users.css"
  div ! class_ "page-header" $ do
    h1 $ "Users"
  div $ ul ! class_ "media-list" ! id "index-users" $ do
    forM_ ps $ \user -> do
      let userUrl = "/users/" `T.append` userId user
      li ! class_ "media"
         ! onclick (toValue $ T.concat ["location.href=\'",userUrl,"\'"]) $ do
        img ! class_ "pull-left media-object"
            ! src (toValue $ T.concat ["https://secure.gravatar.com/avatar/"
                                      , md5 (userEmail user), "?s=48"])
        div ! class_ "media-body" $ do
            h4 ! class_ "media-heading" $ do
              a ! href (toValue userUrl) $ toHtml $
               T.concat $ (userId user) :
                 if T.null (userFullName user)
                   then []
                   else [" [ ", userFullName user, " ] "]
            when (musr == Just user) $ do
              a ! class_ "pull-right"
                ! href (toValue $ userUrl `T.append` "/edit") $ do
                 i ! class_ "icon-wrench" $ ""
                 " edit "

showUser :: User -> [Post] -> Bool -> Html
showUser user ps isCurrentUser = do
  div ! class_ "page-header" $ do
    ul ! class_ "media-list " $ do
      li ! class_ "media" $ do
        img ! class_ "pull-left media-object"
            ! src (toValue $ T.concat ["https://secure.gravatar.com/avatar/"
                                      , md5 (userEmail user), "?s=48"])
        div ! class_ "media-body" $ do
            h4 ! class_ "media-heading" $ toHtml $ userId user
            toHtml $ userFullName user
    when (isCurrentUser) $ do
      a ! class_ "btn btn-primary"
        ! href (toValue $ T.concat ["/users/", userId user, "/edit"]) $ do
        i ! class_ "icon-wrench icon-white" $ ""
        " Edit account"
  ul ! class_ "nav nav-pills nav-stacked" $ do
    forM_ ps $ \post -> do
      let postUrl = "/posts/" ++ show (getPostId post)
      li $ do
          a ! href (toValue postUrl) $ do
            i ! class_ (snd $ privInfo post) $ ""
            " "
            toHtml $ postTitle post
            span ! class_ "pull-right" $ toHtml $ showDate (postDate post)
  where privInfo p = if postIsPublic p
                       then ("Public post", "icon-globe")
                       else ("Private post", "icon-lock")

editUser :: User -> Html
editUser usr = do
  h1 $ toHtml (userId usr)
  div $ do
    form ! action "/users" ! method "POST" ! id "editUser" $ do
      div $ do
        input ! type_ "hidden" ! name "_method" ! value "PUT"
        input ! type_ "hidden" ! name "_id"
              ! value (toValue $ userId usr)
      div $ do
        label ! for "fullName" $ "Full name:"
        input ! class_ "span4" ! type_ "text"
              ! name "fullName" ! id "fullName"
              ! value (toValue $ userFullName usr)
      div $ do
        label ! for "gravatar" $ "Email:"
        input ! class_ "span4" ! type_ "email"
              ! id "editUser-email"
              ! name "email" ! id "email"
              ! value (toValue $ userEmail usr)
      div ! class_ "btn-group" $ do
        input ! type_ "submit" ! class_ "btn btn-primary" ! value "Done"
        input ! type_ "reset" ! class_ "btn" ! value "Reset"


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

-- | MD5 hash
md5 :: Text -> Text
md5 = T.pack . show . MD5.md5 . L8.pack . T.unpack