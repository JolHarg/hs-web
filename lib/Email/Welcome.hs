{-# LANGUAGE OverloadedStrings #-}

module Email.Welcome where

import Data.Text.Lazy
import Email.Email                 as Email
import Network.Mail.Mime
import Text.Blaze.Html5            ((!))
import Text.Blaze.Html5            qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Types.Email                 as TypeEmail
import Types.Name
import Types.User

welcome ∷ String → User → Email.Email
welcome host User {
    email = UserEmail {
        getUserEmail = TypeEmail.Email {
            getEmail = email'
        }
    },
    name = UserName {
        getUserName = Name {
            getName = name'
        }
    },
    verificationToken = UserVerificationToken {
        getUserVerificationToken = Nothing
    }
} = Email.Email {
    to = Address (Just name') email',
    subject = "Welcome to JobFinder!",
    text = "Hi " <> fromStrict name' <> ", welcome to JobFinder! Thanks for verifying your email address. You can now log in at: " <> pack host,
    html = H.p $ do
        "Hi " <> H.text name' <> ", welcome to JobFinder! Thanks for verifying your email address. You can now log in at: "
        (H.a ! A.href (H.lazyTextValue $ pack host))
            . H.lazyText $ pack host
}
welcome _ User {
    verificationToken = UserVerificationToken {
        getUserVerificationToken = Just _
    }
} = error "Verification token not present; should not be sending email."
