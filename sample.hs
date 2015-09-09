import Yesod
import Yesod.Form.Jquery
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>))
import Control.Monad (forM_)

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
      setPNotify defaultPNotify { _title = Right (mkTitle JqueryUI Notice)
                                , _text = Right "Look at my beautiful styling! ^_^"
                                , _styling = JqueryUI
                                , _type = Notice
                                }
      setPNotify defaultPNotify { _title = Right (mkTitle Bootstrap3 Success)
                                , _text = Right "Look at my beautiful styling! ^_^"
                                , _styling = Bootstrap3
                                , _type = Success
                                }
      setPNotify defaultPNotify { _title = Right (mkTitle BrightTheme Error)
                                , _text = Right "Look at my beautiful styling! ^_^"
                                , _styling = BrightTheme
                                , _type = Error
                                }
      setPNotify defaultPNotify { _title = Right (mkTitle FontAwesome Info)
                                , _text = Right "Look at my beautiful styling! ^_^"
                                , _styling = FontAwesome
                                , _type = Info
                                }
      redirect PersonR
    _ -> do
      setPNotify defaultPNotify { _title = Right (mkTitle JqueryUI Notice)
                                , _text = Right "Look at my beautiful styling! ^_^"
                                , _styling = JqueryUI
                                , _type = Notice
                                }
      redirect PersonR
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
main = warp 3000 Devel
