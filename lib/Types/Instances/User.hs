-- TODO move this somewhere that makes sense
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.Instances.User () where

import Servant.Auth.JWT
import Types.User

instance FromJWT User
instance ToJWT User
