{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}


module Server.Users where

import Control.Monad.Reader
import DB.Instances.User          ()
import DB.SQLite
import Servant
import Types.API.Users
import Types.App
import Types.Env
import Types.User                 as User

getUsersAPI ∷ User → App GetUsersAPI
getUsersAPI _user = do
    conn' <- asks conn
    getAllSoftDeletedInclusive conn' "users_view"

getUserIdAPI ∷ User → App GetUserIdAPI
getUserIdAPI _user' userId = do
    conn' <- asks conn
    mUser <- getOneByIdSoftDeletedInclusive conn' "users_view" userId
    case mUser of
        Just user -> pure user
        _ -> throwError $ err404 {
            errBody = "Specified user not found"
        }

deleteUserIdAPI ∷ User → App DeleteUserIdAPI
deleteUserIdAPI _user userId = do
    conn' <- asks conn
    softDeleteById conn' "users" "deletedAt" userId
    pure "Nothing"

putUserIdAPI ∷ User → App PutUserIdAPI
putUserIdAPI _user userId newUser = do
    conn' <- asks conn
    mUser <- getOneByIdSoftDeletedInclusive conn' "users_view" userId :: AppM (Maybe User)
    case mUser of
        Just _ -> do
            mUpdatedUser <- updateOneByIdSoftDeleteInclusive conn' "users" "users_view" newUser :: AppM (Maybe User)
            case mUpdatedUser of
                Nothing -> throwError $ err404 { 
                    errBody = "Specified user not found"
                }
                Just updatedUser -> pure updatedUser
        _ -> throwError $ err404 {
            errBody = "Specified user not found"
        }

postUserAPI ∷ User → App PostUserAPI
postUserAPI _user createUser = do
    conn' <- asks conn
    insertOne conn' "users" "users_view" createUser
    
usersAPI ∷ User → App UsersAPI
usersAPI user =
    getUsersAPI user :<|>
    getUserIdAPI user :<|>
    deleteUserIdAPI user :<|>
    putUserIdAPI user :<|>
    postUserAPI user
