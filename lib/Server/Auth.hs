{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Server.Auth where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Crypto.BCrypt
import Data.Maybe
import Data.Text.Encoding
import Data.UUID.V4
import DB.Instances.User       ()
import DB.SQLite
import Email.Email             (sendEmail)
import Email.Verify
import Email.Welcome
-- import           HaveIBeenPwned
import Servant
import Servant.Auth.Server
import Types.API.Auth
import Types.App
import Types.Email             as Email
import Types.Env
import Types.Instances.User    ()
import Types.Login             as Login
import Types.Name              as Name
import Types.Password
import Types.Register          as Register
import Types.User              (CreateUser (CreateUser), User)
import Types.User              qualified as CreateUser (CreateUser (..),
                                                        CreateUserEmail (..),
                                                        CreateUserName (..),
                                                        CreateUserPassword (..),
                                                        CreateUserType (..),
                                                        CreateUserUsername (..),
                                                        CreateUserVerificationToken (..))
import Types.User              qualified as User (User (..), UserPassword (..),
                                                  UserVerificationToken (..))
import Types.Username
import Types.UserType
import Types.VerificationToken

-- Here is the login handler
loginAPI ∷ CookieSettings
    → JWTSettings
    → Login
    → AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Maybe User))
loginAPI cookieSettings jwtSettings Login {
    Login.username = Username username',
    Login.password = Password password'
} = do
    conn' <- asks conn
    -- Usually you would ask a database for the user info. This is just a
    -- regular servant handler, so you can follow your normal database access
    -- patterns (including using 'enter').

    -- Valid user?
    mUser <- liftIO $ getOneByFieldSoftDeletedInclusive conn' "users_view" "username" username'

    when (isNothing mUser) $ throwError err401

    let (Just user) = mUser

    let (User.UserPassword (Password storedPassword')) = User.password user

    unless (validatePassword (encodeUtf8 storedPassword') (encodeUtf8 password')) $
        throwError err401

    let mToken = User.getUserVerificationToken (User.verificationToken user)

    when (isJust mToken) $ throwError err403

    mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user

    when (isNothing mApplyCookies) $ throwError err401

    let (Just applyCookies) = mApplyCookies

    pure $ applyCookies (Just user)
{-}
mkRegister ∷ Email → Username → Name → Password → Password → Either ErrorMessage Register
mkRegister email' username' name' pw1 pw2 = do
    when (pw1 /= pw2) . errMsg $ "Passwords not identical"
    when ((16 <) . Text.length $ getPassword pw1) $ errMsg "Password not 8 characters or more."
    pure $ Register {
        Register.email = email',
        Register.username = username',
        Register.name = name',
        Register.password = pw1
    }
-}

registerAPI ∷ App RegisterAPI
registerAPI Register {
    Register.username = Username username',
    Register.email = Email email',
    Register.name = Name name',
    Register.password = Password password'
} = do
    conn' <- asks conn
    smtpSettings' <- asks smtpSettings
    uiHost' <- asks uiHost

    -- Existing user?
    mExistingUserByUsername <- liftIO $ getOneByFieldSoftDeletedExclusive conn' "users" "deletedAt" "username" username' :: AppM (Maybe User)

    mExistingUserByEmail <- liftIO $ getOneByFieldSoftDeletedExclusive conn' "users" "deletedAt" "email" email' :: AppM (Maybe User)

    when (isJust mExistingUserByUsername || isJust mExistingUserByEmail) $
        throwError err409

--     runNoLoggingT $ do
--         mgr <- liftIO $ newManager tlsManagerSettings
--         let hibpEnv = HaveIBeenPwnedConfig mgr "https://api.pwnedpasswords.com/range"
--         p' <- flip runPwnedT hibpEnv $ haveIBeenPwned $ password'
--         liftIO $ case p' of
--             HaveIBeenPwnedResult_Secure ->
--                 pure ()
--             HaveIBeenPwnedResult_Pwned p'' ->
--                 throwError err400 -- include message
--             HaveIBeenPwnedResult_Error ->
--                 throwError err500

    -- hash password
    mHashedPassword <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 password')

    when (isNothing mHashedPassword) $ throwError err500

    let (Just bsHashedPassword) = mHashedPassword

    verificationToken' <- liftIO nextRandom

    -- eh, it's just a PASSWORD, whatcha gonna do, SUE ME?
    let hashedPassword = decodeUtf8Lenient bsHashedPassword

    -- Create user
    -- todo catcherror cont error
    -- todo verification, mkEmail, mkPassword etc
    let userToInsert = CreateUser {
        CreateUser.name = CreateUser.CreateUserName (Name name'),
        CreateUser.username = CreateUser.CreateUserUsername (Username username'),
        CreateUser.email = CreateUser.CreateUserEmail (Email email'),
        CreateUser.password = CreateUser.CreateUserPassword (Password hashedPassword),
        CreateUser.userType = CreateUser.CreateUserType Normal,
        CreateUser.verificationToken = CreateUser.CreateUserVerificationToken (Just (VerificationToken verificationToken'))
    }
    returnedUser <- insertOne conn' "users" "users_view" userToInsert :: AppM User
    liftIO . sendEmail smtpSettings' $ verify uiHost' returnedUser
    pure returnedUser

-- TODO verify
verifyAPI ∷ App VerifyAPI
verifyAPI Nothing = pure NoContent
verifyAPI (Just verificationToken') = do
    conn' <- asks conn
    smtpSettings' <- asks smtpSettings
    uiHost' <- asks uiHost
    mUser <- liftIO $ getOneByFieldSoftDeletedExclusive conn' "users" "deletedAt" "verificationToken" verificationToken' :: AppM (Maybe User)
    when (isJust mUser) $ do
        let (Just user) = mUser
        let modifiedUser = user {
            User.verificationToken = User.UserVerificationToken Nothing
        }
        -- liftIO $ updateOneByIdSoftDeleteExclusive conn'  "users" "users_view" "deletedAt" modifiedUser
        liftIO . sendEmail smtpSettings' $ welcome uiHost' modifiedUser
    pure NoContent

-- TODO forgot / change
forgotAPI ∷ App ForgotAPI
forgotAPI _ = pure NoContent

authAPI ∷ CookieSettings → JWTSettings → App AuthAPI
authAPI cs jwts = loginAPI cs jwts :<|> registerAPI :<|> verifyAPI :<|> forgotAPI
