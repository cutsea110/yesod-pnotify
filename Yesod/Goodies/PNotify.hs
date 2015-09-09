module Yesod.Goodies.PNotify 
       ( PNotify(..)
       , NotifyType(..)
       , NotifyStyling(..)
       , YesodJqueryPnotify(..)
       , getPNotify
       , setPNotify
         -- Utility
       , pnotify
       ) where

import Yesod
import Yesod.Form.Jquery hiding (urlJqueryJs, urlJqueryUiCss)

import Control.Monad (mzero)
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON(..), ToJSON(..), encode, decode)
import Data.Aeson.Parser (value)
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8, encodeUtf8)
import Text.Julius (RawJS(..))

data PNotify = PNotify 
               { sty :: NotifyStyling
               , typ :: NotifyType
               , ttl :: Text
               , msg :: Text
               }
             deriving (Show, Read, Eq)

instance FromJSON PNotify where
  parseJSON (Object v) = PNotify <$>
                         v .: "styling" <*>
                         v .: "type" <*>
                         v .: "title" <*>
                         v .: "text"
  parseJSON _ = mzero

instance ToJSON PNotify where
  toJSON (PNotify sty typ ttl msg) = object ["styling" .= sty
                                            ,"type" .= typ
                                            ,"title" .= ttl
                                            ,"text" .= msg
                                            ]

instance RawJS [PNotify] where
  rawJS = rawJS . TL.decodeUtf8 . encode

data NotifyType = Notice | Info | Success | Error
                deriving (Eq)

instance Read NotifyType where
  readsPrec d r = do
    (v, s') <- lex r
    return $ case v of
      "notice" -> (Notice, s')
      "info" -> (Info, s')
      "success" -> (Success, s')
      "error" -> (Error, s')
      _ -> error $ "invalid NotifyType: " ++ v

instance Show NotifyType where
  show Notice = "notice"
  show Info = "info"
  show Success = "success"
  show Error = "error"

instance FromJSON NotifyType where
  parseJSON (String v) = return $ read $ T.unpack v

instance ToJSON NotifyType where
  toJSON Notice = String "notice"
  toJSON Info = String "info"
  toJSON Success = String "success"
  toJSON Error = String "error"

data NotifyStyling = JqueryUI | Bootstrap3 | BrightTheme | FontAwesome
                   deriving (Eq)

instance Read NotifyStyling where
  readsPrec d r = do
    (v, s') <- lex r
    return $ case v of
      "jqueryui" -> (JqueryUI, s')
      "bootstrap3" -> (Bootstrap3, s')
      "brighttheme" -> (BrightTheme, s')
      "fontawesome" -> (FontAwesome, s')
      _ -> error $ "invalid NotifyStyling: " ++ v

instance Show NotifyStyling where
  show JqueryUI = "jqueryui"
  show Bootstrap3 = "bootstrap3"
  show BrightTheme = "brighttheme"
  show FontAwesome = "fontawesome"

instance FromJSON NotifyStyling where
  parseJSON (String v) = return $ read $ T.unpack v

instance ToJSON NotifyStyling where
  toJSON JqueryUI = String "jqueryui"
  toJSON Bootstrap3 = String "bootstrap3"
  toJSON BrightTheme = String "brighttheme"
  toJSON FontAwesome = String "fontawesome"

class YesodJquery a => YesodJqueryPnotify a where
  urlJqueryJs :: a -> Either (Route a) Text
  urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"
  urlJqueryUiCss :: a -> Either (Route a) Text
  urlJqueryUiCss _ = Right "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/smoothness/jquery-ui.css"
  
  urlPnotifyJs :: a -> Either (Route a) Text
  urlPnotifyJs _ = Right "//cdnjs.cloudflare.com/ajax/libs/pnotify/2.1.0/pnotify.core.min.js"
  urlPnotifyCss :: a -> Either (Route a) Text
  urlPnotifyCss _ = Right "//cdnjs.cloudflare.com/ajax/libs/pnotify/2.1.0/pnotify.core.min.css"

  urlBootstrap3Js :: a -> Either (Route a) Text
  urlBootstrap3Js _ = Right "//netdna.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
  urlBootstrap3Css :: a -> Either (Route a) Text
  urlBootstrap3Css _ = Right "//netdna.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"

  urlBrightThemeCss :: a -> Either (Route a) Text
  urlBrightThemeCss _ = Right "//cdnjs.cloudflare.com/ajax/libs/pnotify/2.1.0/pnotify.brighttheme.min.css"

  urlFontAwesomeCss :: a -> Either (Route a) Text
  urlFontAwesomeCss _ = Right "//netdna.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"

notifyKey :: Text
notifyKey = "_PNotify"

toText :: [PNotify] -> Text
toText = TL.toStrict . TL.decodeUtf8 . encode

fromText :: Text -> [PNotify]
fromText = fromJust . decode . TL.encodeUtf8 . TL.fromStrict

_setPNotify :: [PNotify] -> HandlerT site IO ()
_setPNotify = setSession notifyKey . toText

getPNotify :: HandlerT site IO (Maybe [PNotify])
getPNotify = runMaybeT $ do
  ns <- MaybeT $ lookupSession notifyKey
  lift $ deleteSession notifyKey
  return $ fromText ns


setPNotify :: PNotify -> HandlerT site IO ()
setPNotify n = do
  mns <- getPNotify
  _setPNotify (n:maybe [] id mns)

optionalLoadJsCss :: (MonadWidget m, YesodJqueryPnotify (HandlerSite m)) =>
                     HandlerSite m -> [PNotify] -> m()
optionalLoadJsCss y = sequence_ . map trans . nub . map sty
    where
      trans s = case s of
        JqueryUI
          -> addStylesheetEither $ urlJqueryUiCss y
        Bootstrap3
          -> do { addScriptEither $ urlBootstrap3Js y
                ; addStylesheetEither $ urlBootstrap3Css y
                }
        BrightTheme
          -> addStylesheetEither $ urlBrightThemeCss y
        FontAwesome
          -> addStylesheetEither $ urlFontAwesomeCss y

pnotify :: YesodJqueryPnotify site => site -> WidgetT site IO ()
pnotify y = do
  mnotify <- handlerToWidget getPNotify
  case mnotify of
    Nothing -> return ()
    Just ps -> do
      addScriptEither $ urlJqueryJs y
      addScriptEither $ urlPnotifyJs y
      addStylesheetEither $ urlPnotifyCss y

      optionalLoadJsCss y ps

      toWidget [julius|$(function(){var ws=#{rawJS ps};for(var i in ws){new PNotify(ws[i]);}});|]
