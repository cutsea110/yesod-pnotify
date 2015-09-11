import Control.Applicative ((<$>),(<*>))
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T

import Yesod
import Yesod.Form.Jquery hiding (urlJqueryJs)
import Yesod.Form.Bootstrap3
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
      addScriptEither $ urlJqueryJs y
      addScriptEither $ urlBootstrap3Js y
      addStylesheetEither $ urlBootstrap3Css y
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
      <div .container>
        <div .row>
          ^{pageBody pc}
|]

instance YesodJquery Demo
instance YesodJqueryPnotify Demo
instance RenderMessage Demo FormMessage where
    renderMessage _ _ = defaultFormMessage


data Account = Account { ident :: Text, passwd :: Text } deriving Show
accountForm :: AForm Handler Account
accountForm = Account
              <$> areq textField (bfs ("Id" :: Text)) Nothing
              <*> areq passwordField (bfs ("Pass" :: Text)) Nothing
              <*  bootstrapSubmit ("Sign in" :: BootstrapSubmit Text)

hGrid = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)

getLoginR :: Handler Html
getLoginR = do
  (w, e) <- generateFormPost $ renderBootstrap3 hGrid accountForm
  defaultLayout $ do
    setTitle "Login"
    [whamlet|
     <form .form-horizontal role=form method=post action=@{LoginR} enctype=#{e}>
       <div .form-group>
         <div .col-sm-4>
         <h1 .col-sm-6>Sign in
       ^{w}
    |]

postLoginR :: Handler Html
postLoginR = do
  ((r, w), e) <- runFormPost $ renderBootstrap3 hGrid accountForm
  case r of
    FormSuccess acc ->
      if ident acc == passwd acc
      then do
        setPNotify $ defaultPNotify { _type = Just Success
                                    , _styling = Just Bootstrap3
                                    , _title = Just $ Right "Hello"
                                    , _text = Just $ Right $ "Welcome, " `T.append` ident acc
                                    }
        redirect (HomeR $ ident acc)
      else do
        setPNotify $ defaultPNotify { _type = Just Error
                                    , _styling = Just Bootstrap3
                                    , _title = Just $ Right "Try again"
                                    , _text = Just $ Right "Please match the Id and the Pass."
                                    }
        redirect LoginR
    _ -> do
      setPNotify $ defaultPNotify { _type = Just Error
                                  , _styling = Just Bootstrap3
                                  , _title = Just $ Right "Fail"
                                  , _text = Just $ Right "What happen?"
                                  }
      redirect LoginR

getHomeR :: Text -> Handler Html
getHomeR name = defaultLayout $ do
  setTitle "Home"
  [whamlet|
   <p>`#{name}' Logged in.
   <a href=@{LoginR} .btn .btn-primary>Sign out
  |]

main :: IO ()
main = warp 3000 Demo
