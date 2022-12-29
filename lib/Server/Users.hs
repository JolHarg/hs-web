{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}


module Server.Users where

import           Control.Monad.Trans.Reader
import           DB.Instances.User          ()
import           DB.SQLite
import           Servant
import           Types.API.Users
import           Types.App
import           Types.Env
import           Types.User                 as User

getUsersAPI ∷ User → App GetUsersAPI
getUsersAPI _user = do
    conn' <- asks conn
    getAll conn' "users"

getUserIdAPI ∷ User → App GetUserIdAPI
getUserIdAPI _user' userId = do
    conn' <- asks conn
    mUser <- getOneById conn' "users" userId
    case mUser of
        Just user -> pure user
        _ -> throwError $ err404 {
            errBody = "Specified user not found"
        }

deleteUserIdAPI ∷ User → App DeleteUserIdAPI
deleteUserIdAPI _user userId = do
    conn' <- asks conn
    deleteById conn' "users" userId
    pure "Nothing"

putUserIdAPI ∷ User → App PutUserIdAPI
putUserIdAPI _user userId newUser = do
    conn' <- asks conn
    mUser <- getOneById conn' "users" userId :: AppM (Maybe User)
    case mUser of
        Just _ -> do
            updateOneById conn' "users" newUser
            pure newUser
        _ -> throwError $ err404 {
            errBody = "Specified user not found"
        }

postUserAPI ∷ User → App PostUserAPI
postUserAPI _user newUser = do
    conn' <- asks conn
    insertOne conn' "users" newUser
    pure newUser

usersAPI ∷ User → App UsersAPI
usersAPI user =
    getUsersAPI user :<|>
    getUserIdAPI user :<|>
    deleteUserIdAPI user :<|>
    putUserIdAPI user :<|>
    postUserAPI user
