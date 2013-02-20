{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , OverloadedStrings #-}
module LBH.Controllers where

import qualified Data.ByteString.Char8 as S8

import           Control.Monad

import           LIO
import           LIO.DCLabel

import           Hails.Data.Hson (ObjectId, labeledRequestToHson)
import           Hails.Database (select)
import           Hails.Database.Structured
import           Hails.HttpServer.Types
import           Hails.Web
import           Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import qualified Hails.Web.Frank as Frank

import           LBH.MP
import           LBH.Views

import Debug.Trace

server :: Application
server = mkRouter $ do
  routeTop $ redirectTo "/posts/"
  routeName "posts" postsController
  Frank.get "/login" $ withUserOrDoAuth $ \_ -> redirectBack

postsController :: RESTController
postsController = do
  REST.index $ do
    mu <- getHailsUser
    ps <- liftLIO . withLBHPolicy $ findAll $ select [] "posts"
    return $ respondHtml mu $ indexPosts mu ps :: Controller Response
  REST.new $ withUserOrDoAuth $ \u ->
    return $ respondHtml (Just u) (newPost u)
  REST.create $ withUserOrDoAuth $ const $ do
    lreq <- request 
    liftLIO . withLBHPolicy $ do
      lpost <- liftLIO $ labeledRequestToPost lreq
      void $ insertLabeledRecord lpost
      return $ redirectTo $ "/posts/"
  REST.show $ do
    mu <- getHailsUser
    (Just pid) <- queryParam "id"
    mpost <- liftLIO . withLBHPolicy $ do
      let _id = read . S8.unpack $ pid :: ObjectId
      findBy "posts" "_id" _id
    return $ maybe notFound (respondHtml mu . showPost mu) mpost
  REST.edit $ withUserOrDoAuth $ \usr -> do
    (Just pid) <- queryParam "id"
    mpost <- liftLIO . withLBHPolicy $ do
      let _id = read . S8.unpack $ pid :: ObjectId
      findBy "posts" "_id" _id
    return $ maybe notFound (respondHtml (Just usr) . editPost usr) mpost
  REST.update $ withUserOrDoAuth $ \usr -> do
    trace (show "UPDATE") $ return ()
    ldoc <- request >>= labeledRequestToHson
    mlpost <- liftLIO $ partiallyFillPost ldoc
    case mlpost of
      Nothing -> return serverError
      Just lpost -> do liftLIO $ savePost lpost
                       post <- unlabel lpost
                       return $ redirectTo $ "/posts/" ++ show (getPostId post)