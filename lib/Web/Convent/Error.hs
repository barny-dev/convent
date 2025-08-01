module Web.Convent.Error where

data Error = Error { errorMessage :: String, errorCause :: Maybe Error }

wrap error newMessage =  Error { errorMessage = newMessage, errorCause = Just error}