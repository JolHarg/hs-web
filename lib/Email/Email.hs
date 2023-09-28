module Email.Email where

import Control.Monad.IO.Class
import Data.Text                     as T
import Data.Text.Lazy                as TL
import Network.Mail.Mime             (htmlPart, plainPart)
import Network.Mail.SMTP             (sendMailWithLoginSTARTTLS', simpleMail)
import Network.Mail.SMTP.Types
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5              (Html)
import Types.Env

data Email = Email {
    to      :: Address,
    subject :: String,
    text    :: TL.Text,
    html    :: Html
}

noTextTemplate ∷ TL.Text → TL.Text
noTextTemplate = id

noHtmlTemplate ∷ Html → Html
noHtmlTemplate = id

-- TODO reader env probably
sendEmail ∷ MonadIO m ⇒ SMTPSettings → Email → m ()
sendEmail SMTPSettings {
    server = server',
    port = port',
    --tls = tls',
    username = username',
    password = password',
    fromEmail = fromEmail',
    fromName = fromName'
} Email {
    to = to',
    subject = subject',
    text = text',
    html = html'
} = liftIO .
    -- TODO deal with ssl /
    sendMailWithLoginSTARTTLS' server' port' username' password' $
    simpleMail
        (Address (Just (T.pack fromName')) (T.pack fromEmail'))
        [to']
        []
        []
        (T.pack subject')
        [
            plainPart text',
            htmlPart $ renderHtml html'
        ]

-- sendEmailWithTemplate :: ...
