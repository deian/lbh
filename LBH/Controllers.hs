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

import           LBH.ActiveHaskell
import           Data.Aeson (decode, encode)

import Debug.Trace

server :: Application
server = mkRouter $ do
  routeTop $ redirectTo "/posts/"
  routeName "posts" postsController
  routeName "users" usersController
  Frank.get "/tags/:tag" tagsController
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
        let _id = getPostId post
        return $ redirectTo $ "/posts/" ++ show _id

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
  if ct /= Just "text/json"
    then return badRequest
    else do obj <- decode `liftM` body
            case obj of
              Nothing -> return badRequest
              Just c -> do
                r <- liftLIO $ execCode c
                return $ ok "text/json" (encode r)

--
-- Search
--

tagsController :: Controller Response
tagsController = do
  mu <- currentUser
  Just tag <- queryParam "tag"
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