
module Types.Env where

import Database.SQLite.Simple
import Network.Socket

data SMTPSettings = SMTPSettings {
    server    :: HostName,
    port      :: PortNumber,
    tls       :: Bool,
    username  :: String,
    password  :: String,
    fromEmail :: String,
    fromName  :: String
}

data Env = Env {
    conn         :: Connection,
    smtpSettings :: SMTPSettings,
    apiHost      :: HostName,
    apiPort      :: PortNumber,
    uiHost       :: HostName
}
