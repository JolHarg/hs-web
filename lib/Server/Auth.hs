{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Server.Auth where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Crypto.BCrypt
import Data.Maybe
import Data.Text.Encoding
import Data.Time
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
import Types.User              as User
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
    mUser <- liftIO $ getOneByFieldSoftDeletedExclusive conn' "users" "deletedAt" "username" username'

    when (isNothing mUser) $ throwError err401

    let (Just user) = mUser

    let (UserPassword (Password storedPassword')) = User.password user

    unless (validatePassword (encodeUtf8 storedPassword') (encodeUtf8 password')) $
        throwError err401

    let mToken = getUserVerificationToken (User.verificationToken user)

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

    userId <- liftIO nextRandom
    verificationToken' <- liftIO nextRandom
    now <- liftIO getCurrentTime
    let hashedPassword = decodeUtf8 bsHashedPassword
    -- Create user
    -- todo catcherror cont error
    -- todo verification, mkEmail, mkPassword etc
    let userToInsert = User {
        User.id = UserId userId,
        User.name = UserName (Name name'),
        User.username = UserUsername (Username username'),
        User.email = UserEmail (Email email'),
        User.password = UserPassword (Password hashedPassword),
        User.userType = UserType Normal,
        User.verificationToken = UserVerificationToken (Just (VerificationToken verificationToken')),
        User.createdAt = UserCreatedAt now,
        User.updatedAt = UserUpdatedAt Nothing,
        User.deletedAt = UserDeletedAt Nothing
    }
    insertOne conn' "users" userToInsert
    liftIO . sendEmail smtpSettings' $ verify uiHost' userToInsert
    pure userToInsert

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
            verificationToken = UserVerificationToken Nothing
        }
        -- liftIO $ updateOneByIdSoftDeleteExclusive conn'  "users" "deletedAt" modifiedUser
        liftIO . sendEmail smtpSettings' $ welcome uiHost' modifiedUser
    pure NoContent

-- TODO forgot / change
forgotAPI ∷ App ForgotAPI
forgotAPI = undefined

authAPI ∷ CookieSettings → JWTSettings → App AuthAPI
authAPI cs jwts = loginAPI cs jwts :<|> registerAPI :<|> verifyAPI :<|> forgotAPI
