{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , OverloadedStrings #-}
module LBH.Controllers where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import           Data.Maybe

import           Control.Monad

import           LIO
import           LIO.DCLabel
import           LIO.Concurrent

import           Hails.Data.Hson (ObjectId, labeledRequestToHson)
import           Hails.Database
import           Hails.Database.Structured
import           Hails.HttpServer.Types
import           Hails.Web
import           Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import qualified Hails.Web.Frank as Frank

import           LBH.MP
import           LBH.Views

import           LBH.ActiveCode
import           Data.Aeson (decode, encode)

import Debug.Trace

server :: Application
server = mkRouter $ do
  routeTop $ redirectTo "/posts/"
  routeName "posts" postsController
  routeName "users" usersController
  routeName "tags"   tagsController
  Frank.post "/exec" execController
  Frank.get "/login" $ withAuthUser $ \_ -> redirectBack

--
-- Posts
--

postsController :: RESTController
postsController = do
  REST.index $ do
    mu <- currentUser
    ps <- liftLIO . withLBHPolicy $ findAll $ select [] "posts"
    ups <- liftLIO $ forM ps $ \p-> do
      u <- withLBHPolicy $ findBy  "users" "_id" (postOwner p)
      return (fromMaybe (User { userId = postOwner p
                              , userFullName = T.empty
                              , userEmail = T.empty }) u, p)
    return $ respondHtml mu $ indexPosts "All posts" mu ups
  REST.new $ withAuthUser $ \u ->
    return $ respondHtml (Just u) (newPost u)
  REST.create $ withAuthUser $ const $ do
    lreq <- request 
    liftLIO . withLBHPolicy $ do
      lpost <- liftLIO $ labeledRequestToPost lreq
      _id <- insertLabeledRecord lpost
      -- insert tags
      post <- unlabel lpost
      forM_ (postTags post) $ \t -> do
        md <- findBy "tags" "_id" t
        case md of
          Nothing -> void $ insertRecord TagEntry { tagName = t, tagCount = 1 }
          Just t' -> saveRecord $ t' { tagCount = tagCount t' + 1 }
      --
      return $ redirectTo $ "/posts/" ++ (show _id)
  REST.show $ do
    mu <- currentUser
    (Just pid) <- queryParam "id"
    mpost <- liftLIO . withLBHPolicy $ do
      let _id = read . S8.unpack $ pid :: ObjectId
      findBy "posts" "_id" _id
    return $ maybe notFound (respondHtml mu . showPost mu) mpost
  REST.edit $ withAuthUser $ \usr -> do
    (Just pid) <- queryParam "id"
    mpost <- liftLIO . withLBHPolicy $ do
      let _id = read . S8.unpack $ pid :: ObjectId
      findBy "posts" "_id" _id
    return $ maybe notFound (respondHtml (Just usr) . editPost) mpost
  REST.update $ withAuthUser $ \usr -> do
    ldoc <- request >>= labeledRequestToHson
    mlpost <- liftLIO $ partiallyFillPost ldoc
    case mlpost of
      Nothing -> return serverError
      Just lpost -> do
        liftLIO $ savePost lpost
        post <- unlabel lpost
        return $ redirectTo $ "/posts/" ++ show (getPostId post)

--
-- Users
--

usersController :: RESTController
usersController = do
  REST.index $ do
    mu <- currentUser
    ps <- liftLIO . withLBHPolicy $ findAll $ select [] "users"
    return $ respondHtml mu $ indexUsers mu ps
  REST.new $ withAuthUser $ \u ->
    return $ redirectTo $ "/users/" ++ T.unpack (userId u) ++ "/edit"
  REST.show $ do
    mu <- currentUser
    (Just uid) <- queryParam "id"
    (muser, ps) <- liftLIO . withLBHPolicy $ do
      muser <- findBy "users" "_id" uid
      ps <- maybe (return [])
                  (\o -> findAll $ select ["owner" -: userId o] "posts") muser
      return (muser, ps)
    return $ maybe notFound
                   (respondHtml mu . (\u -> showUser u ps (mu==muser))) muser
  REST.edit $ withAuthUser $ \usr -> do
    (Just uid) <- queryParam "id"
    if (T.unpack (userId usr) /= S8.unpack uid)
      then return forbidden
      else do muser <- liftLIO . withLBHPolicy $ findBy "users" "_id" uid
              return $ maybe notFound (respondHtml (Just usr) . editUser) muser
  REST.update $ withAuthUser $ \usr -> do
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withLBHPolicy $ do
      luser <- fromLabeledDocument ldoc
      void $ saveLabeledRecord luser
      user <- unlabel luser
      return $ redirectTo $ "/users/" ++ T.unpack (userId user)

--
-- Code execution
--

execController :: Controller Response
execController = do
  ct <- requestHeader "content-type"
  if ct /= Just "application/json"
    then return badRequest
    else do obj <- decode `liftM` body
            case obj of
              Nothing -> return badRequest
              Just c -> do
                r <- liftLIO $ execCode c
                return $ ok "application/json" (encode r)

--
-- Search
--

tagsController :: RESTController
tagsController = do
  REST.index $ do
    mu <- currentUser
    ts <- liftLIO . withLBHPolicy $ do
      findAll $ (select [] "tags") { sort = [Asc "count"] }
    matype <- requestHeader "accept"
    case matype of
      Just atype |  "application/json" `S8.isInfixOf` atype ->
           return $ ok "application/json" (encode $ tagsToJSON ts)
      _ -> return $ respondHtml mu $ indexTags ts
  REST.show $ do
    mu <- currentUser
    Just tag <- queryParam "id"
    ups <- liftLIO . withLBHPolicy $ do
      let qry :: BsonDocument
          qry = ["$in" -: [tag]]
      ps <- findAll (select ["tags" -: qry] "posts")
      forM ps $ \p-> do
        u <- findBy  "users" "_id" (postOwner p)
        return (fromMaybe (User { userId = postOwner p
                                , userFullName = T.empty
                                , userEmail = T.empty }) u, p)
    return $ respondHtml mu $ indexPosts (S8.unpack tag ++ " posts") mu ups