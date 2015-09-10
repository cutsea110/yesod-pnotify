import Prelude hiding (Either(..))
import qualified Prelude as Prelude (Either(..))

import Control.Applicative ((<$>),(<*>))
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T

import Yesod
import Yesod.Form.Jquery
import Yesod.Goodies.PNotify

data Demo = Demo

mkYesod "Demo" [parseRoutes|
/ HomeR GET POST
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

getHomeR :: Handler Html
getHomeR = do
  x <- newIdent
  defaultLayout $ do
    setTitle "PNotify sample"
    [whamlet|
<h1>This is a sample</h1>
<form method=post action=@{HomeR}>
  <label for=#{x}>PNotify</label>
  <input id=#{x} type=submit value="Click Me!">
|]

postHomeR :: Handler Html
postHomeR = do
  forM_ [defaultPNotify { _title = Just $ Prelude.Right $ mkTitle s t
                        , _text = Just $ Prelude.Right "Look at my beautiful styling! ^_^"
                        , _styling = Just s
                        , _type = Just t
                        }
        | t <- [Notice ..]
        , s <- [JqueryUI ..]
        ] setPNotify
  redirect HomeR
  where
    fromStyling :: NotifyStyling -> Text
    fromStyling JqueryUI = "jQuery UI"
    fromStyling Bootstrap3 = "Bootstrap"
    fromStyling BrightTheme = "Bright Theme"
    fromStyling FontAwesome = "Font Awesome"

    fromType Notice = "Notice"
    fromType Info = "Info"
    fromType Success = "Success"
    fromType Error = "Error"

    mkTitle s t = fromStyling s `T.append` " " `T.append` fromType t


main :: IO ()
main = warp 3000 Demo
