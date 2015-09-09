module Yesod.Goodies.PNotify 
       ( module Yesod.Goodies.PNotify.Types
       , module Yesod.Goodies.PNotify.Types.Instances
       , PNotify(..)
       , YesodJqueryPnotify(..)
       , getPNotify
       , setPNotify
         -- Utility
       , pnotify
       , defaultPNotify
       ) where

import Yesod
import Yesod.Form.Jquery hiding (urlJqueryJs, urlJqueryUiCss)

import Control.Monad (mzero)
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON(..), ToJSON(..), encode, decode)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8, encodeUtf8)
import Text.Julius (RawJS(..))

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances

data PNotify = PNotify
               { _title                    :: Either Bool Text
--               , _title_escape             :: Either Bool Text
               , _text                     :: Either Bool Text
--               , _text_escape              :: Either Bool Text
               , _styling                  :: NotifyStyling
--               , _addclass                 :: Text
--               , _cornerclass              :: Text
--               , _auto_display             :: Bool
--               , _width                    :: Text
--               , _min_height               :: Text
               , _type                     :: NotifyType
--               , _icon                     :: Either Bool Text
--               , _animation                :: AnimationType
--               , _animate_speed            ::AnimateSpeed
--               , _position_animate_speed   :: Int
--               , _opacity                  :: Double
--               , _shadow                   :: Bool
--               , _hide                     :: Bool
--               , _delay                    :: Int
--               , _mouse_reset              :: Bool
--               , _remove                   :: Bool
--               , _insert_brs               :: Bool
               }
             deriving (Show, Read, Eq, Ord)
instance FromJSON PNotify where
  parseJSON (Object v) = PNotify <$>
                         v .: "title" <*>
--                         v .: "title_escape" <*>
                         v .: "text" <*>
--                         v .: "text_escape" <*>
                         v .: "styling" <*>
--                         v .: "addclass" <*>
--                         v .: "cornerclass" <*>
--                         v .: "auto_display" <*>
--                         v .: "width" <*>
--                         v .: "min_height" <*>
                         v .: "type" -- <*>
--                         v .: "icon" <*>
--                         v .: "animation" <*>
--                         v .: "animate_speed" <*>
--                         v .: "position_animate_speed" <*>
--                         v .: "opacity" <*>
--                         v .: "shadow" <*>
--                         v .: "hide" <*>
--                         v .: "delay" <*>
--                         v .: "mouse_reset" <*>
--                         v .: "remove" <*>
--                         v .: "insert_brs"
  parseJSON _ = mzero

instance ToJSON PNotify where
  toJSON (PNotify { _title
--                  , _title_escape
                  , _text
--                  , _text_escape
                  , _styling
--                  , _addclass
--                  , _cornerclass
--                  , _auto_display
--                  , _width
--                  , _min_height
                  , _type
--                  , _icon
--                  , _animation
--                  , _animate_speed
--                  , _position_animate_speed
--                  , _opacity
--                  , _shadow
--                  , _hide
--                  , _delay
--                  , _mouse_reset
--                  , _remove
--                  , _insert_brs
                  })
      = object [ "title"                    .= _title
--               , "title_escape"             .= _title_escape
               , "text"                     .= _text
--               , "text_escape"              .= _text_escape
               , "styling"                  .= _styling
--               , "addclass"                 .= _addclass
--               , "cornerclass"              .= _cornerclass
--               , "auto_display"             .= _auto_display
--               , "width"                    .= _width
--               , "min_height"               .= _min_height
               , "type"                     .= _type
--               , "icon"                     .= _icon
--               , "animation"                .= _animation
--               , "animate_speed"            .= _animate_speed
--               , "position_animate_speed"   .= _position_animate_speed
--               , "opacity"                  .= _opacity
--               , "shadow"                   .= _shadow
--               , "hide"                     .= _hide
--               , "delay"                    .= _delay
--               , "mouse_reset"              .= _mouse_reset
--               , "remove"                   .= _remove
--               , "insert_brs"               .= _insert_brs
               ]


defaultPNotify :: PNotify
defaultPNotify = PNotify
                 { _title                   = Left False
--                 , _title_escape            = Left False
                 , _text                    = Left False
--                 , _text_escape             = Left False
                 , _styling                 = BrightTheme
--                 , _addclass                = T.empty
--                 , _cornerclass             = T.empty
--                 , _auto_display            = True
--                 , _width                   = "300px"
--                 , _min_height              = "16px"
                 , _type                    = Notice
--                 , _icon                    = Left True
--                 , _animation               = Fade
--                 , _animate_speed           = Slow
--                 , _position_animate_speed  = 500
--                 , _opacity                 = 1.0
--                 , _shadow                  = True
--                 , _hide                    = True
--                 , _delay                   = 8000
--                 , _mouse_reset             = True
--                 , _remove                  = True
--                 , _insert_brs              = True
                 }

instance RawJS [PNotify] where
  rawJS = rawJS . TL.decodeUtf8 . encode

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
fromText = maybe [] id . decode . TL.encodeUtf8 . TL.fromStrict

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
optionalLoadJsCss y = sequence_ . map trans . nub . map _styling
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

      toWidget [julius|$(function(){alert(#{rawJS ps});$.each(#{rawJS ps},function(i,v){new PNotify(v)});});|]
