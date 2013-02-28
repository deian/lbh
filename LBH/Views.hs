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
import           Data.Aeson ((.=), toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Digest.Pure.MD5 as MD5
import           Hails.Web hiding (body)
import           Hails.HttpServer.Types
import           Text.Blaze.Html5 hiding (Tag)
import           Text.Blaze.Html5.Attributes hiding (label, form, span, title, style)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8

import qualified Text.Pandoc.Readers.Markdown as P
import qualified Text.Pandoc.Writers.HTML as P
import qualified Text.Pandoc.Options as P
import qualified Text.Pandoc.Highlighting as P

import           LBH.ActiveCode (extractActieCodeBlocks)

import Debug.Trace

respondHtml :: Maybe User -> Html -> Response
respondHtml muser content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    title "Learn By Hacking"
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
    script ! src "/static/js/jquery.min.js" $ ""
    script ! src "/static/js/jquery.cookie.js" $ ""
    script ! src "/static/js/bootstrap.min.js" $ ""
    script ! src "https://login.persona.org/include.js" $ ""
    script ! src "/static/js/application.js" $ ""
  body $ do
     div ! class_ "navbar navbar-fixed-top navbar-inverse"
         ! id "page-nav" $ do
       div ! class_ "navbar-inner" $ do
         div ! class_ "container" $ do
           a ! href "/" ! class_ "brand" $ "Learn By Hacking" 
           ul ! class_ "nav pull-right" $
             maybe publicMenu userMenu muser
     div ! class_ "container" $ do
       div ! id "main-alert" ! class_ "alert alert-error"
           ! A.style "display: none" $ do
         button ! type_ "button" ! class_ "close"
                ! dataAttribute "dismiss" "alert" $ 
                preEscapedToHtml ("&times;" :: Text)
         span ! id "main-alert-msg" $ ""
       div $ content
      where publicMenu = do
              li $ a ! href "#" ! id "login" $ do
               span ! class_ "icon-user icon-white" $ ""
               " Login"
            userMenu u = do
              li ! class_ "dropdown" $ do
                a ! href "#" ! class_ "dropdown-toggle" 
                  ! dataAttribute "toggle" "dropdown" $ do
                    img ! src (toValue $ T.concat [
                      "https://secure.gravatar.com/avatar/"
                      , md5 (userEmail u), "?s=16"])
                    " "
                    toHtml $ userId u
                    b ! class_ "caret" $ ""
                ul ! class_ "dropdown-menu" $ do
                  let uUrl = "/users/" `T.append` userId u
                  li $ a ! href (toValue uUrl) $ do
                    span ! class_ "icon-user" $ ""
                    " View Profile"
                  li $ a ! href (toValue $ uUrl `T.append` "/edit") $ do
                    span ! class_ "icon-edit" $ ""
                    " Edit Profile"
                  li $ a ! href "/posts/new" $ do
                    span ! class_ "icon-folder-open" $ ""
                    " New Post"
                  li $ a ! href "#" ! id "logout" $ do
                    span ! class_ "icon-road" $ ""
                    " Logout"

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
              ! autocomplete "off"
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
        input ! type_ "hidden" ! name "owner"
              ! value (toValue $ postOwner post)
        forM_ (postCollaborators post) $ \c ->
          input ! type_ "hidden" ! name "collaborators[]"
                ! value (toValue c)
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
              ! autocomplete "off"
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
        a ! class_ "btn"
          ! id "post-preview-btn"
          ! dataAttribute "toggle" "button" $ do
            i ! class_ "icon-eye-open" $ ""
            " Preview"
      div ! class_ "btn-group pull-right dropup" $ do
        button ! class_ "btn" 
               ! dataAttribute "toggle" "dropdown" $ do
                    i ! class_ "icon-wrench" $ ""
        ul ! class_ "dropdown-menu" $ do
          li $ a ! (if postIsPublic post
                      then A.style "display: none"
                      else mempty)
                 ! id "post-make-public-btn" $ do
                   i ! class_ "icon-globe" $ ""
                   " Make public"
          li $ a ! (if not $ postIsPublic post
                      then A.style "display: none"
                      else mempty)
                 ! id "post-make-private-btn" $ do
                   i ! class_ "icon-lock" $ ""
                   " Make private"
          li $ a ! href "#manageCollabs"
                 ! dataAttribute "toggle" "modal" $ do
                 i ! class_ "icon-user" $ ""
                 " Manage collaborators"
          li ! class_ "divider" $ ""
          li $ a ! href "#confirmDelete"
                 ! dataAttribute "toggle" "modal" $ do
                 i ! class_ "icon-trash" $ ""
                 " Delete"
  div ! id "post-preview" ! A.style "display: none" $ do
    ul ! class_ "breadcrumb" $ do
       li $ "Preview..."
       li ! class_ "pull-right" $ do
         a ! href "#" ! id "post-refresh-preview-btn" $
           i ! class_ "icon-refresh" $ ""
    iframe ! id "post-preview-body"
           ! src (toValue $ "/posts/" ++ (show $ getPostId post))
           $ ""
  -- Delete confirmation modal
  div ! id "confirmDelete" ! class_ "modal hide fade" ! tabindex "-1" $ do
    div ! class_ "modal-header" $ do
      button ! type_ "button" ! class_ "close"
             ! dataAttribute "dismiss" "modal" $ 
             preEscapedToHtml ("&times;" :: Text)
      h3 $ "Are you sure you want to delete post?"
    div ! class_ "modal-body" $ do
     p $ "Once you delete this post, you will not be able to\
         \ recover its contents."
     div ! class_ "btn-group pull-right" $ do
       button ! type_ "button" ! class_ "btn"
              ! dataAttribute "dismiss" "modal" $ "Cancel"
       button ! type_ "button" ! class_ "btn btn-danger"
              ! id "post-delete-btn" $ "Delete"
  -- Collaborators modal
  div ! id "manageCollabs" ! class_ "modal hide fade" ! tabindex "-1" $ do
    div ! class_ "modal-header" $ do
      button ! type_ "button" ! class_ "close"
             ! dataAttribute "dismiss" "modal" $ "x"
      h3 $ "Manage collaborators"
      a ! href "#"
        ! class_ "text-warning"
        ! id "post-add-collaborator-help"
        ! dataAttribute "toggle" "popover"
        ! dataAttribute "original-title" "What is a collaborator?"
        ! dataAttribute "content"
          "A collaborator can read, modify, and delete your post.\
          \ They may also add and remove other collaborators.\
          \ However, a collaborator cannot remove or change the\
          \ owner."
         $ small "What is a collaborator?"
    div ! class_ "modal-body" $ do
      ul ! class_ "nav nav-list" ! id "currentCollabs" $ do
        li ! class_ "nav-header" $ "Owner"
        li $ a ! href "#" $ toHtml $ postOwner post
        li ! class_ "divider" $ ""
        li ! class_ "nav-header" $ "Collaborators"
        forM_ (postCollaborators post) $ \c ->
          li ! id (toValue $ "collaborator-" `T.append` c) $ a ! href "#" $ do
                 toHtml c
                 span ! class_ "pull-right collaborator-remove"
                      ! dataAttribute "collaborator" (toValue c) $
                   i ! class_ "icon-trash" $ ""
               
    div ! class_ "modal-footer" $  do
      div ! class_ "input-append inline" $ do
        input ! type_ "text"
              ! id "post-add-collaborator"
              ! class_ "span3"
              ! placeholder "New collaborator"
              ! dataAttribute "provide" "typeahead"
        button ! type_ "button"
               ! id "post-add-collaborator-btn"
               ! class_ "btn btn-primary "
               ! disabled "" $ do
          i ! class_ "icon-plus icon-white" $ ""
          " Add"

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
      when (isJust muser &&
            (userId . fromJust $ muser)
                 `elem` (postOwner post : postCollaborators post)) $ do
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
       in extractActieCodeBlocks md
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
              a ! href (toValue userUrl) $ toHtml $ userId user
            when (musr == Just user) $ do
              a ! class_ "pull-right"
                ! href (toValue $ userUrl `T.append` "/edit") $ do
                 i ! class_ "icon-wrench" $ ""
                 " edit "
            div $ toHtml $ userFullName user

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

newUser :: UserName -> Html
newUser uemail = do
  script ! src "/static/js/application/users.js" $ ""
  h1 "Register"
  div $ do
    form ! action "/users" ! method "POST" ! id "newUser" $ do
      div $ do
        label ! for "emailD" $ "Email address:"
        input ! class_ "span4" ! type_ "text"
              ! name "emailD" ! id "emailD"
              ! disabled "disabled"
              ! value (toValue uemail)
        input ! type_ "hidden"
              ! name "email" ! id "email"
              ! value (toValue uemail)
      div $ do
        label ! for "fullName" $ "Full name:"
        input ! class_ "span4" ! type_ "text"
              ! name "fullName" ! id "fullName"
              ! placeholder "King Schultz"
      div ! class_ "control-group" ! id "_id-group" $ do
        label ! class_ "control-label" ! for "_id" $ do
          "Username ("
          a ! href "#"
            ! dataAttribute "toggle" "tooltip"
            ! A.title "Usernames can be at most 16 characters \
                      \long. They must start with a letter and \
                      \only contain letters, numbers, and \'_\',\
                      \i.e., it must match ^[a-zA-Z][a-zA-Z0-9_]+$"
            $ "?"
          "):"
        input ! class_ "span4" ! type_ "text"
              ! name "_id" ! id "_id"
              ! placeholder "dr_schultz"
        span ! id "_id-group-help"
             ! class_ "help-inline" $ ""
      div ! class_ "btn-group" $ do
        input ! type_ "button" ! id "newUser-submit-btn"
              ! class_ "btn btn-primary" ! value "Sign Up"
        input ! type_ "reset"
              ! id "newUser-reset-btn"
              ! class_ "btn" ! value "Reset"

editUser :: User -> Html
editUser user = do
  script ! src "/static/js/application/users.js" $ ""
  h1 $ toHtml (userId user)
  div $ do
    form ! action "/users" ! method "POST" ! id "editUser" $ do
      input ! type_ "hidden" ! name "_method" ! value "PUT"
      div $ do
        label ! for "emailD" $ "Email address:"
        input ! class_ "span4" ! type_ "text"
              ! name "emailD"
              ! disabled "disabled"
              ! value (toValue $ userEmail user)
        input ! type_ "hidden"
              ! name "email" ! id "email"
              ! value (toValue $ userEmail user)
      div $ do
        label ! for "fullName" $ "Full name:"
        input ! class_ "span4" ! type_ "text"
              ! name "fullName" ! id "fullName"
              ! value (toValue $ userFullName user)
      div ! class_ "control-group" ! id "_id-group" $ do
        label ! class_ "control-label" ! for "_id" $ do
          "Username:"
        input ! class_ "span4" ! type_ "text"
              ! name "_idD"
              ! value (toValue $ userId user)
              ! disabled "disabled"
        input ! type_ "hidden"
              ! name "_id" ! id "_id"
              ! value (toValue $ userId user)
        span ! id "_id-group-help"
             ! class_ "help-inline" $ ""
      div ! class_ "btn-group" $ do
        input ! type_ "submit" ! class_ "btn btn-primary" ! value "Update"
        input ! type_ "reset" ! class_ "btn" ! value "Reset"
--
-- Tags
--

indexTags :: [TagEntry] -> Html
indexTags ts = do
  div ! class_ "page-header" $ do
    h1 $ "Tags"
  div $ if null ts
    then p $ "Sorry, no tags... :-("
    else table ! class_ "table table-hover table-condensed" $ do
         thead $ tr $ do
           th $ "#"
           th $ "Tag"
           th $ "Count"
         tbody $ do
           forM_ (zip [1..] ts) $ \(nr,tag) -> do
             let tagUrl = "/tags/" ++ T.unpack (tagName tag)
             tr ! onclick (toValue $ "location.href=" ++ show tagUrl )$ do
               td $ toHtml (nr :: Int)
               td $ toHtml $ tagName tag
               td $ toHtml $ tagCount tag

tagsToJSON :: [TagEntry] -> Aeson.Value
tagsToJSON ts = toJSON $ Aeson.object [ "tags" .= ts]

--
-- Login
--

loginPage :: Html
loginPage = do
  script $ "$(document).ready(function() { $(\"#login\").click(); });"
  "Please login first..."

--
-- Helper functions
--


-- | Show time
showDate :: UTCTime -> String
showDate = showGregorian . utctDay
                    
-- | Replace all CR-LFs to LFs, so the Markdown parser works fine
crlf2lf :: Text -> Text
crlf2lf = T.unlines . lines'
                    

-- | MD5 hash
md5 :: Text -> Text
md5 = T.pack . show . MD5.md5 . L8.pack . T.unpack