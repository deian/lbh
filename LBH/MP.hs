{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , DeriveDataTypeable
           , MultiParamTypeClasses #-}
-- | Defines the data model and security policy of posts.
module LBH.MP ( personaLoginEmailToUid
              , withLBHPolicy
                -- * Posts
              , PostId
              , getPostId
              , Post(..), labeledRequestToPost, partiallyFillPost 
              , savePost, deletePost
                -- * Users
              , User(..), createUser, updateUser
                -- * Tags
              , Tag, TagEntry(..)
              ) where

import           Prelude hiding (lookup)

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as S8
import           Data.Typeable
import           Data.Time.Clock (UTCTime)
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Aeson (ToJSON(..), (.=), object)
import           Text.Regex.Posix

import           Control.Monad

import           Hails.Data.Hson
import           Hails.Web hiding (body)
import           Hails.Database
import           Hails.Database.Structured
import           Hails.PolicyModule
import           Hails.PolicyModule.DSL
import           Hails.HttpServer.Types

import           LIO
import           LIO.DCLabel
import           LIO.DCLabel.Core
import           LIO.DCLabel.Privs.TCB (allPrivTCB)

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
               collabs = map userToPrincipal $ postOwner p : postCollaborators p
               ws      =  List.foldl' (\/) this collabs
           readers ==> if postIsPublic p
                         then anybody
                         else ws
           writers ==> ws
         field "owner" $ searchable
         field "tags"  $ searchable
         field "collaborators" $ searchable
       -- = Users ===================================================
       collection "users" $ do
         access $ do
           readers ==> anybody
           writers ==> (this \/ principal "#users")
         clearance $ do
           secrecy   ==> this
           integrity ==> anybody
         document $ \doc -> do
           let (Just u) = fromDocument doc
               user = userToPrincipal . userEmail $ u
           readers ==> anybody
           writers ==> this \/ user
         field "fullName" $ searchable
         field "email"    $ searchable
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
data Post = Post { postId            :: PostId
                 , postTitle         :: Text
                 , postOwner         :: UserName
                 , postDescription   :: Text
                 , postBody          :: Text
                 , postIsPublic      :: Bool
                 , postDate          :: UTCTime
                 , postTags          :: [Tag]
                 , postCollaborators :: [UserName]
                 } deriving (Show, Eq)


instance DCRecord Post where
  fromDocument doc = do
    let pid      = lookupObjId "_id" doc
        isPublic = fromMaybe False $ lookupBool "isPublic" doc
        tags     = fromMaybe [] $ (lookupTyped "tags"  doc :: Maybe [Tag])
        collabs  = fromMaybe [] $ (lookupTyped "collaborators"  doc
                                    :: Maybe [UserName])
    title        <- lookup "title" doc
    owner        <- lookup "owner" doc
    description  <- lookup "description" doc
    body         <- lookup "body"  doc
    date         <- lookup "date"  doc
    return Post { postId            = pid
                , postTitle         = title
                , postOwner         = owner
                , postDescription   = description
                , postBody          = body
                , postDate          = date
                , postIsPublic      = isPublic
                , postTags          = tags 
                , postCollaborators = collabs }
                
  toDocument p = 
    let pre = maybe [] (\i -> ["_id" -: i]) $ postId p
    in pre ++ [ "title"         -: postTitle p
              , "owner"         -: postOwner p
              , "description"   -: postDescription p
              , "body"          -: postBody p
              , "date"          -: postDate p
              , "isPublic"      -: postIsPublic p
              , "tags"          -: postTags p
              , "collaborators" -: postCollaborators p ]

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
                           ["tags" -: ([]::[Tag])
                           ,"collaborators" -: ([]::[UserName])
                           ] `merge` doc'
          return (Just res)
        _ -> return Nothing
  where safeFields = [ "isPublic", "title", "description"
                     , "body", "tags", "collaborators"]

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
                 , userEmail    :: UserName  -- ^ User's email
                 } deriving (Show, Eq)

instance DCRecord User where
  fromDocument doc = do
    uid      <- lookup "_id" doc
    fullName <- lookup "fullName" doc
    email    <- lookup "email" doc
    return User { userId       = uid
                , userFullName = fullName
                , userEmail    = email }
                
  toDocument u = [ "_id"      -: userId u
                 , "fullName" -: userFullName u
                 , "email"    -: userEmail u ]

  recordCollection _ = "users"

instance DCLabeledRecord LBHPolicy User where
  endorseInstance _ = LBHPolicyTCB noPriv


-- | Inser new user. Nothing = success, String = error
createUser :: DCLabeled User -> DC (Maybe String)
createUser luser = withPolicyModule $ \(LBHPolicyTCB _privs) -> do
  let (Just privs) = dcDelegatePriv _privs (privDesc _privs \/ principal "#users")
  user  <- unlabel luser
  let wellFormed = let uid = T.unpack $ userId user
                   in length uid <= 16 &&
                      uid =~ ("^[a-zA-Z][a-zA-Z0-9_]+$" :: String)
  if not wellFormed
    then return . Just $ "Malformed username"
    else do ps0 <- findAll (select ["_id"   -: userId user] "users")
            ps1 <- findAll (select ["email" -: userEmail user] "users")
            if null (ps0 ++ ps1 :: [User])
              then do void $ insertLabeledRecordP privs luser
                      return Nothing
              else return . Just $ "Username exists"

-- | Update user. Use least privilege and user partiallyFillUser to
-- assert that only the name can be modified.
updateUser :: DCLabeled Document -> DC (Maybe (DCLabeled User))
updateUser ldoc = withPolicyModule $ \(LBHPolicyTCB _privs) -> do
  mluser <- partiallyFillUser _privs ldoc
  case mluser of
    Nothing -> return Nothing
    Just luser -> do
      let (Just privs) = dcDelegatePriv _privs
                                        (privDesc _privs \/ principal "#users")
      saveLabeledRecordP privs luser
      return (Just luser)

-- | Create new record with partially filled fields
partiallyFillUser :: DCPriv
                  -> DCLabeled Document -> DBAction (Maybe (DCLabeled User))
partiallyFillUser p ldoc = do
  -- Unlabel document
  doc <- unlabel ldoc
  case lookup "_id" doc of
    Nothing -> return Nothing
    Just (_id :: UserName) -> do
      -- Lookup existing document:
      mldoc' <- findOne (select ["_id" -: _id] "users")
      clr <- getClearance
      case mldoc' of
        Just ldoc' | canFlowTo (labelOf ldoc') clr -> do
          res <- liftLIO $ toLabeledTCB p (labelOf ldoc') $ do
            doc' <- unlabel ldoc'
            -- merge new (safe) fields and convert it to a record
            fromDocument $ (safeFields `include` doc) `merge` doc'
          return (Just res)
        _ -> return Nothing
  where safeFields = [ "fullName"]

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

-- | Requests are labeled by email addreses, relabel to id's.
personaLoginEmailToUid :: Middleware
personaLoginEmailToUid app conf lreq = do
  meu <- liftLIO . withPolicyModule $ \(LBHPolicyTCB privs) -> do
    let i     = dcIntegrity $ requestLabel conf
        s     = dcSecrecy   $ requestLabel conf
        ps    = toList i
        email = head . head $ ps
    -- Label must have format <_, principal>
    musr <- if i == dcFalse || length ps /= 1 || length (head ps) /= 1
              then return Nothing
              else do mu <- findByP privs "users" "email" $
                              T.decodeUtf8 . principalName $ email
                      return $ (principal . T.encodeUtf8 . userId) `liftM` mu
    return $ do { u <- musr ; return $ mkXfms email u }
  case meu of
    Nothing -> app conf lreq
    Just (e2u, u2e) -> do
         let conf' = conf { browserLabel = dcLabel
                              (e2u $ dcSecrecy $ browserLabel conf) dcTrue
                          , requestLabel = dcLabel
                               dcTrue (e2u $ dcIntegrity $ requestLabel conf)
                          }
         setClearanceP allPrivTCB $ browserLabel conf'
         lreq' <- relabelLabeledP allPrivTCB (requestLabel conf') lreq
         resp <- app conf' lreq'
         curl <- getLabel
         curc <- getClearance
         let curc' = dcLabel (u2e $ dcSecrecy curc) (u2e $ dcIntegrity curc)
             curl' = dcLabel (u2e $ dcSecrecy curl) (u2e $ dcIntegrity curl)
         -- raise current clearance, considering the current label
         setClearanceP allPrivTCB $ curc' `upperBound` curl
         -- change current label
         setLabelP allPrivTCB curl'
         -- lower current clearance
         setClearanceP allPrivTCB $ curc'
         return resp
    where mkXfms e u = ( \cmp -> xfm (\x -> if x == e then u else x) cmp
                       , \cmp -> xfm (\x -> if x == u then e else x) cmp )
          xfm f cmp | cmp == dcTrue || cmp == dcFalse = cmp
          xfm f cmp = let cs = unDCFormula cmp
                      in dcFormula $
                           Set.map (\c -> Clause $ Set.map f $ unClause c) cs