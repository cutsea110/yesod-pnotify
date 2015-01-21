{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

import Yesod
import Yesod.Form.Jquery
import Data.Text (Text)
import Control.Applicative ((<$>),(<*>))
import Text.Blaze.Html (preEscapedToHtml)

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
    giveUrlRenderer [hamlet|
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
      let updateMsg = "<b>Update</b> User profile." :: Text
          info = "<i>Information</i>" :: Text
          infoMsg = "And <u>more</u> information." :: Text
      setPNotify $ PNotify JqueryUI Success "Updated" $ preEscapedToHtml updateMsg
      setPNotify $ PNotify JqueryUI Notice "Notice" "More notice. <no tag>"
      setPNotify $ PNotify JqueryUI Info (preEscapedToHtml info) $ preEscapedToHtml infoMsg
      redirect PersonR
    _ -> do
      setPNotify $ PNotify JqueryUI Error "Error" "Fail to update user profile"
      redirect PersonR

main :: IO ()
main = warp 3000 Devel
