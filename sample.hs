{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module Devel where

import Yesod
import Yesod.Form.Jquery
import Data.Text (Text)
import Data.Monoid ((<>), mempty)
import Control.Applicative ((<$>),(<*>))

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans.Maybe
import Data.Char (toLower)

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
    hamletToRepHtml [hamlet|
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
personForm :: Html -> MForm Devel Devel (FormResult Person, Widget)
personForm = renderDivs $ Person
             <$> areq textField "Name" Nothing
             <*> areq intField "Age" Nothing

getPersonR :: Handler RepHtml
getPersonR = do
  (widget, enctype) <- generateFormPost personForm
  defaultLayout [whamlet|
<form method=post action=@{PersonR} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
|]

postPersonR :: Handler RepHtml
postPersonR = do
  ((result, widget), enctype) <- runFormPost personForm
  case result of
    FormSuccess person -> do 
      setPNotify $ PNotify JqueryUI Success "Updated" "ユーザー情報を更新しました."
      setPNotify $ PNotify JqueryUI Error "Error" "テストでエラー発生."
      setPNotify $ PNotify JqueryUI Notice "Notice" "注意せよ."
      redirect PersonR
    _ -> do
      setPNotify $ PNotify JqueryUI Error "Error" "ユーザーの更新に失敗しました."
      redirect PersonR

main :: IO ()
main = warpDebug 3000 Devel
