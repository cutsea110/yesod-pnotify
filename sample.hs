import Yesod
import Yesod.Form.Jquery
import Data.Text (Text)
import Control.Applicative ((<$>),(<*>))

import Yesod.Goodies.PNotify

data Devel = Devel

mkYesod "Devel" [parseRoutes|
/ PersonR GET POST
|]



instance Yesod Devel where
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

instance YesodJquery Devel
instance YesodJqueryPnotify Devel where

instance RenderMessage Devel FormMessage where
  renderMessage _ _ = defaultFormMessage

data Person = Person { name :: Text
                     , age :: Int
                     }
              deriving (Show)
personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
             <$> areq textField "Name" Nothing
             <*> areq intField "Age" Nothing

getPersonR :: Handler Html
getPersonR = do
  (widget, enctype) <- generateFormPost personForm
  defaultLayout [whamlet|
<form method=post action=@{PersonR} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
|]

postPersonR :: Handler Html
postPersonR = do
  ((result, _), _) <- runFormPost personForm
  case result of
    FormSuccess _ -> do

      setPNotify $ PNotify JqueryUI Success "Updated" "Update User profile."
      setPNotify $ PNotify JqueryUI Notice "Notice" "More notice."
      setPNotify $ PNotify JqueryUI Info "Information" "And more information."
      setPNotify $ PNotify JqueryUI Error "Error" "Fail to update user profile"
      
      setPNotify $ PNotify Bootstrap3 Success "Updated" "Update User profile."
      setPNotify $ PNotify Bootstrap3 Notice "Notice" "More notice."
      setPNotify $ PNotify Bootstrap3 Info "Information" "And more information."
      setPNotify $ PNotify Bootstrap3 Error "Error" "Fail to update user profile"
      
      setPNotify $ PNotify BrightTheme Success "Updated" "Update User profile."
      setPNotify $ PNotify BrightTheme Notice "Notice" "More notice."
      setPNotify $ PNotify BrightTheme Info "Information" "And more information."
      setPNotify $ PNotify BrightTheme Error "Error" "Fail to update user profile"

      setPNotify $ PNotify FontAwesome Success "Updated" "Update User profile."
      setPNotify $ PNotify FontAwesome Notice "Notice" "More notice."
      setPNotify $ PNotify FontAwesome Info "Information" "And more information."
      setPNotify $ PNotify FontAwesome Error "Error" "Fail to update user profile"

      redirect PersonR
    _ -> do
      setPNotify $ PNotify JqueryUI Error "Error" "Fail to update user profile"
      setPNotify $ PNotify Bootstrap3 Error "Error" "Fail to update user profile"
      setPNotify $ PNotify BrightTheme Error "Error" "Fail to update user profile"
      redirect PersonR

main :: IO ()
main = warp 3000 Devel
