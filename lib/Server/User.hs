module Server.User where

-- import           Control.Monad.Reader
import DB.Instances.User ()
-- import           DB.SQLite
import Servant
import Types.API.User
import Types.App
-- import           Types.Env
import Types.User        as User

getUserAPI ∷ User → App GetUserAPI
getUserAPI = pure

deleteUserAPI ∷ User → App DeleteUserAPI
deleteUserAPI _user = throwError err501

putUserAPI ∷ User → App PutUserAPI
putUserAPI _user _newUser = throwError err501

userAPI ∷ User → App UserAPI
userAPI user =
    getUserAPI user :<|>
    deleteUserAPI user :<|>
    putUserAPI user
