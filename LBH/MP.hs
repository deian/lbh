{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , DeriveDataTypeable
           , MultiParamTypeClasses #-}
-- | Defines the data model and security policy of posts.
module LBH.MP ( withLBHPolicy
                -- * Posts
              , PostId
              , getPostId
              , Post(..), labeledRequestToPost, partiallyFillPost 
              , savePost, deletePost
                -- * Users
              , User(..), currentUser, withAuthUser
                -- * Tags
              , Tag, TagEntry(..)
              ) where

import           Prelude hiding (lookup)

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import           Data.Typeable
import           Data.Time.Clock (UTCTime)
import qualified Data.List as List
import           Data.Aeson (ToJSON(..), (.=), object)

import           Control.Monad

import           Hails.Data.Hson
import           Hails.Web
import           Hails.Database
import           Hails.Database.Structured
import           Hails.PolicyModule
import           Hails.PolicyModule.DSL
import           Hails.HttpServer.Types

import           LIO
import           LIO.DCLabel

--
-- Policy
--

-- | The type constructor should not be exported to avoid leaking
-- the privilege.
data LBHPolicy = LBHPolicyTCB DCPriv
                  deriving Typeable

instance PolicyModule LBHPolicy where
   initPolicyModule priv =  do
     setPolicy priv $ do
      -- Anybody can read and write to DB
      -- Only MP can administer DB
       database $ do
         readers ==> anybody
         writers ==> anybody
         admins  ==> this
       -- = Posts ===================================================
       collection "posts" $ do
       -- Anybody can write a new post
       -- Only owner of the post can modify it
       -- Post is publicly readable when the owner indicates as such
         access $ do
           readers ==> anybody
           writers ==> anybody
         clearance $ do
           secrecy   ==> this
           integrity ==> anybody
         document $ \doc -> do
           let (Just p) = fromDocument doc
               owner = userToPrincipal . postOwner $ p
           readers ==> if postIsPublic p
                         then anybody
                         else this \/ owner
           writers ==> this \/ owner
         field "owner" $ searchable
         field "tags"  $ searchable
       -- = Users ===================================================
       collection "users" $ do
         access $ do
           readers ==> anybody
           writers ==> anybody
         clearance $ do
           secrecy   ==> this
           integrity ==> anybody
         document $ \doc -> do
           let (Just u) = fromDocument doc
               user = userToPrincipal . userId $ u
           readers ==> anybody
           writers ==> this \/ user
         field "fullName" $ searchable
       -- = Tags ====================================================
       collection "tags" $ do
         access $ do
           readers ==> anybody
           writers ==> anybody
         clearance $ do
           secrecy   ==> this
           integrity ==> anybody
         document $ const $ do
           readers ==> anybody
           writers ==> anybody
         field "count"  $ searchable
       --
     return $ LBHPolicyTCB priv
       where this = privDesc priv
             userToPrincipal = principal . S8.pack . T.unpack



--
-- Posts
--

--- | Post identifiers (@Nothing@ if post is newly create).
type PostId  = Maybe ObjectId

-- | Unsafe post id extrat
getPostId :: Post -> ObjectId
getPostId = fromJust . postId

-- | A tag id is just a string
type Tag = Text

-- | Data type encoding posts.
data Post = Post { postId          :: PostId
                 , postTitle       :: Text
                 , postOwner       :: UserName
                 , postDescription :: Text
                 , postBody        :: Text
                 , postIsPublic    :: Bool
                 , postDate        :: UTCTime
                 , postTags        :: [Tag]
                 } deriving (Show, Eq)


instance DCRecord Post where
  fromDocument doc = do
    let pid      = lookupObjId "_id" doc
        isPublic = fromMaybe False $ lookupBool "isPublic" doc
        tags     = fromMaybe [] $ (lookupTyped "tags"  doc :: Maybe [Tag])
    title        <- lookup "title" doc
    owner        <- lookup "owner" doc
    description  <- lookup "description" doc
    body         <- lookup "body"  doc
    date         <- lookup "date"  doc
    return Post { postId          = pid
                , postTitle       = title
                , postOwner       = owner
                , postDescription = description
                , postBody        = body
                , postDate        = date
                , postIsPublic    = isPublic
                , postTags        = tags }
                
  toDocument p = 
    let pid = postId p
        pre = maybe [] (\i -> ["_id" -: i]) $ postId p
    in pre ++ [ "title"       -: postTitle p
              , "owner"       -: postOwner p
              , "description" -: postDescription p
              , "body"        -: postBody p
              , "date"        -: postDate p
              , "isPublic"    -: postIsPublic p
              , "tags"        -: postTags p ]

  recordCollection _ = "posts"

instance DCLabeledRecord LBHPolicy Post where
  endorseInstance _ = LBHPolicyTCB noPriv


-- | Execute action, restoring the current label.
-- Secrecy of the current label is preserved in the label of the value.
-- The result is partially endorse by the MP's label.
--
-- DO NOT EXPOSE THIS FUNCTION
toLabeledTCB :: DCPriv -> DCLabel -> DC a -> DC (DCLabeled a)
toLabeledTCB privs lgoal act = do
  l0   <- getLabel
  res  <- act
  l1   <- getLabel
  let lendorse = dcLabel dcTrue (toComponent (privDesc privs))
      lgoal' = lgoal `lub` lendorse
            -- effectively use privs in integrity only:
      l1' = l1 `glb` dcLabel dcFalse (toComponent (privDesc privs))
  unless (l1' `canFlowTo` lgoal') $ fail "Invalid usage of toLabeled"
  lres <- labelP privs lgoal' res
  setLabelP privs (partDowngradeP privs l1 l0)
  return lres
   

-- | Convert a labeled reques to  a labeled post, setting the date
labeledRequestToPost :: DCLabeled Request -> DC (DCLabeled Post)
labeledRequestToPost lreq = withPolicyModule $ \(LBHPolicyTCB p) ->
  liftLIO $ toLabeledTCB p (labelOf lreq) $ do
    -- Get labeled document
    ldoc <- labeledRequestToHson lreq
    -- Unlabel request (need time)
    req  <- unlabel lreq
    -- Unlabel document (to add time)
    doc  <- unlabel ldoc
    -- Convert document to record
    fromDocument $ [ "date" -: requestTime req] `merge` doc

-- | Create new record with partially filled fields
partiallyFillPost :: DCLabeled Document -> DC (Maybe (DCLabeled Post))
partiallyFillPost ldoc = withPolicyModule $ \(LBHPolicyTCB p) -> do
  -- Unlabel document
  doc <- unlabel ldoc
  case lookupObjId "_id" doc of
    Nothing -> return Nothing
    Just (_id :: ObjectId) -> do
      -- Lookup existing document:
      mldoc' <- findOne (select ["_id" -: _id] "posts")
      clr <- getClearance
      case mldoc' of
        Just ldoc' | canFlowTo (labelOf ldoc') clr -> do
          res <- liftLIO $ toLabeledTCB p (labelOf ldoc') $ do
            doc' <- unlabel ldoc'
            -- merge new (safe) fields and convert it to a record
            fromDocument $ (safeFields `include` doc) `merge`
                           ["tags" -: ([]::[Tag])] `merge` -- delete (hack)
                           doc'
          return (Just res)
        _ -> return Nothing
  where safeFields = ["isPublic", "title", "description", "body", "tags"]

-- | Save post, by first declassifying it
savePost :: DCLabeled Post -> DC ()
savePost lpost =  withPolicyModule $ \(LBHPolicyTCB privs) -> do
  lpost' <- untaintLabeledP privs l lpost
  post <- unlabelP privs lpost
  (Just lpostE) <- findOne (select ["_id" -: postId post] "posts")
  -- Use privs if integrity stays the same, but we're making
  -- the post private
  let p = if l `canFlowTo` labelOf lpostE
            then privs else noPriv
  saveLabeledRecordP p lpost'
  -- update tags
  postE <- unlabel lpostE >>= fromDocument
  let tags = List.nub $ postTags post ++ postTags postE
      cnts = map (\t -> case () of
                         _ | t `elem`    postTags postE &&
                             t `notElem` postTags post     -> -1
                         _ | t `notElem` postTags postE &&
                             t `elem`    postTags post     ->  1
                         _                                 ->  0) tags
  forM_ (zip tags cnts) $ \(t, c) -> do
    md <- findBy "tags" "_id" t
    case md of
      Nothing -> void $ insertRecordP p TagEntry { tagName = t, tagCount = 1 }
      Just t' -> saveRecordP p $ t' { tagCount = tagCount t' + c }
  --
    where l = dcLabel dcTrue (dcIntegrity . labelOf $ lpost)

-- | Delete post if endorsed by owner
deletePost :: DCLabeled Request -> DC ()
deletePost lreq = withPolicyModule $ \(LBHPolicyTCB privs) -> do
  doc <- labeledRequestToHson lreq >>= unlabel
  case (lookupObjId "_id" doc, lookup "_method" doc) of
    (Just _id, Just ("DELETE" :: Text)) -> do
     (Just lpost) <- findOne $ select ["_id" -: _id] "posts"
     -- Use privs if request is from the correct user (i.e.,
     -- has at least same integrity)
     let p = if l `canFlowTo` labelOf lpost
               then privs else noPriv
     deleteP p (select ["_id" -: _id] "posts")
     -- update tags
     post <- unlabelP p lpost >>= fromDocument
     forM_ (postTags post) $ \t -> do
       (Just t') <- findBy "tags" "_id" t
       saveRecordP p $ t' { tagCount = tagCount t' - 1 }
     --
    _ -> fail "deletePost: Missing _id"
    where l = dcLabel dcTrue (dcIntegrity . labelOf $ lreq)

--
-- Users
--

-- | Data type describing users
data User = User { userId       :: UserName  -- ^ UserName
                 , userFullName :: Text      -- ^ User's full name
                 , userEmail    :: Text      -- ^ User's email MD5(e-mail)
                 } deriving (Show, Eq)

instance DCRecord User where
  fromDocument doc = do
    uid      <- lookup "_id" doc
    fullName <- lookup "fullName" doc
    email <- lookup "email" doc
    return User { userId       = uid
                , userFullName = fullName
                , userEmail    = email }
                
  toDocument u = [ "_id"      -: userId u
                 , "fullName" -: userFullName u
                 , "email"    -: userEmail u ]

  recordCollection _ = "users"

instance DCLabeledRecord LBHPolicy User where
  endorseInstance _ = LBHPolicyTCB noPriv

--
-- Tags
--

-- | Data type describing tag entries
data TagEntry = TagEntry { tagName  :: Tag
                         , tagCount :: Int
                         } deriving (Show, Eq)
                     
instance ToJSON TagEntry where
  toJSON (TagEntry n c) = object [ "tag"   .= n
                                 , "count" .= c ]

instance DCRecord TagEntry where
  fromDocument doc = do
    t <- lookup "_id" doc
    c <- lookup "count" doc
    return TagEntry { tagName = t, tagCount = c }
                
  toDocument t = [ "_id"   -: tagName t
                 , "count" -: tagCount t ]

  recordCollection _ = "tags"

--
-- Misc
--
  

-- | Execute a database action against the posts DB.
withLBHPolicy :: DBAction a -> DC a
withLBHPolicy act = withPolicyModule $ \(LBHPolicyTCB _) -> act

-- | Get object id (may need to convert from string).
lookupObjId :: Monad m => FieldName -> HsonDocument -> m ObjectId
lookupObjId = lookupTyped 

-- | Get boolean (may need to convert from string).
lookupBool  :: Monad m => FieldName -> HsonDocument -> m Bool
lookupBool = lookupTyped 

-- | Generic lookup with possible type cast
lookupTyped :: (HsonVal a, Read a, Monad m) => FieldName -> HsonDocument -> m a
lookupTyped n d = case lookup n d of
    Just i -> return i
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupTyped: cannot extract id from " ++ show n
  where maybeRead = fmap fst . listToMaybe . reads

-- Create LBH user from X-Hails header
currentUser :: Controller (Maybe User)
currentUser = do
  mu <- getHailsUser
  case mu of
    Nothing -> return Nothing
    Just u -> liftLIO $ withPolicyModule $ \(LBHPolicyTCB priv) -> do
      mres <- findByP priv "users" "_id" u
      case mres of
        Just usr -> return (Just usr)
        Nothing -> do insertRecordP priv $ newUser u
                      return . Just $ newUser u
    where newUser u = User { userId = u
                           , userFullName = T.empty
                           , userEmail = T.empty }

-- | Execute action with authenticated user (or force auth)
withAuthUser :: (User -> Controller Response) -> Controller Response
withAuthUser act = withUserOrDoAuth $ const $ do
  mu <- currentUser
  maybe (return serverError) act mu