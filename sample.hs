import Control.Applicative ((<$>),(<*>))
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T

import Yesod
import Yesod.Form.Jquery
import Yesod.Goodies.PNotify

data Demo = Demo

mkYesod "Demo" [parseRoutes|
/            LoginR GET POST
/home/#Text  HomeR  GET
|]

instance Yesod Demo where
  defaultLayout widget = do
    y <- getYesod
    pc <- widgetToPageContent $ do
      widget
      pnotify y
    withUrlRenderer [hamlet|
$doctype 5
<html>
  <head>
      <title>#{pageTitle pc}
      <meta charset=utf-8>
      ^{pageHead pc}
  <body>
      <article>
        ^{pageBody pc}
|]

instance YesodJquery Demo
instance YesodJqueryPnotify Demo
instance RenderMessage Demo FormMessage where
    renderMessage _ _ = defaultFormMessage


data Account = Account { ident :: Text, passwd :: Text } deriving Show
accountForm :: Html -> MForm Handler (FormResult Account, Widget)
accountForm = renderDivs $ Account
              <$> areq textField "Id" Nothing
              <*> areq passwordField "Pass" Nothing

getLoginR :: Handler Html
getLoginR = do
  (w, e) <- generateFormPost accountForm
  defaultLayout $ do
    setTitle "Login"
    [whamlet|<p>Login
     <form method=post action=@{LoginR} enctype=#{e}>
       ^{w}
       <input type=submit value=Login>
     <span>Please input guest/guest for ID/Pass, and Click Login button.
    |]

postLoginR :: Handler Html
postLoginR = do
  ((r, w), e) <- runFormPost accountForm
  case r of
    FormSuccess acc ->
      if ident acc == passwd acc
      then do
        setPNotify $ defaultPNotify { _type = Just Success
                                    , _styling = Just BrightTheme
                                    , _title = Just $ Right "Hello"
                                    , _text = Just $ Right $ "Welcome, " `T.append` ident acc
                                    }
        redirect (HomeR $ ident acc)
      else do
        setPNotify $ defaultPNotify { _type = Just Error
                                    , _styling = Just BrightTheme
                                    , _title = Just $ Right "Try again"
                                    , _text = Just $ Right "Please match the Id and the Pass."
                                    }
        redirect LoginR
    _ -> do
      setPNotify $ defaultPNotify { _type = Just Error
                                  , _styling = Just BrightTheme
                                  , _title = Just $ Right "Fail"
                                  , _text = Just $ Right "What happen?"
                                  }
      redirect LoginR

getHomeR :: Text -> Handler Html
getHomeR name = defaultLayout [whamlet|<p>`#{name}' Logged in.|]

main :: IO ()
main = warp 3000 Demo
