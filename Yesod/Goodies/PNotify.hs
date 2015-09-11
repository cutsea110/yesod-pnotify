module Yesod.Goodies.PNotify 
       ( module Yesod.Goodies.PNotify.Types
       , module Yesod.Goodies.PNotify.Types.Instances
       , module Buttons
       , module Desktop
       , module Nonblock
       , module Stack
       , module History
       , PNotify(..)
       , YesodJqueryPnotify(..)
       , getPNotify
       , setPNotify
         -- Utility
       , pnotify
       , defaultPNotify
       ) where

import Prelude hiding (Either(..))
import qualified Prelude as Prelude (Either(..))

import Yesod
import Yesod.Form.Jquery hiding (urlJqueryJs, urlJqueryUiCss)

import Control.Monad (mzero)
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON(..), ToJSON(..), encode, decode, (.:?))
import Data.List (nub)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8, encodeUtf8)
import Text.Julius (RawJS(..))

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances
import qualified Yesod.Goodies.PNotify.Modules.Buttons as Buttons
import qualified Yesod.Goodies.PNotify.Modules.Desktop as Desktop
import qualified Yesod.Goodies.PNotify.Modules.History as History
import qualified Yesod.Goodies.PNotify.Modules.Nonblock as Nonblock
import qualified Yesod.Goodies.PNotify.Modules.Reference as Reference
import qualified Yesod.Goodies.PNotify.Modules.Stack as Stack

data PNotify = PNotify
               { _title                    :: Maybe (Prelude.Either Bool Text)
               , _title_escape             :: Maybe (Prelude.Either Bool Text)
               , _text                     :: Maybe (Prelude.Either Bool Text)
               , _text_escape              :: Maybe (Prelude.Either Bool Text)
               , _styling                  :: Maybe NotifyStyling
               , _addclass                 :: Maybe Text
               , _cornerclass              :: Maybe Text
               , _auto_display             :: Maybe Bool
               , _width                    :: Maybe Text
               , _min_height               :: Maybe Text
               , _type                     :: Maybe NotifyType
               , _icon                     :: Maybe (Prelude.Either Bool Text)
               , _animation                :: Maybe AnimationType
               , _animate_speed            :: Maybe AnimateSpeed
               , _position_animate_speed   :: Maybe Int
               , _opacity                  :: Maybe Double
               , _shadow                   :: Maybe Bool
               , _hide                     :: Maybe Bool
               , _delay                    :: Maybe Int
               , _mouse_reset              :: Maybe Bool
               , _remove                   :: Maybe Bool
               , _insert_brs               :: Maybe Bool

               , _stack                    :: Maybe Stack.Stack
               , _buttons                  :: Maybe Buttons.Buttons
               , _desktop                  :: Maybe Desktop.Desktop
               , _history                  :: Maybe History.History
               , _nonblock                 :: Maybe Nonblock.Nonblock
               , _reference                :: Maybe Reference.Reference
               }
             deriving (Show, Read, Eq, Ord)

instance FromJSON PNotify where
  parseJSON (Object v) = PNotify <$>
                         v .:? "title" <*>
                         v .:? "title_escape" <*>
                         v .:? "text" <*>
                         v .:? "text_escape" <*>
                         v .:? "styling" <*>
                         v .:? "addclass" <*>
                         v .:? "cornerclass" <*>
                         v .:? "auto_display" <*>
                         v .:? "width" <*>
                         v .:? "min_height" <*>
                         v .:? "type" <*>
                         v .:? "icon" <*>
                         v .:? "animation" <*>
                         v .:? "animate_speed" <*>
                         v .:? "position_animate_speed" <*>
                         v .:? "opacity" <*>
                         v .:? "shadow" <*>
                         v .:? "hide" <*>
                         v .:? "delay" <*>
                         v .:? "mouse_reset" <*>
                         v .:? "remove" <*>
                         v .:? "insert_brs" <*>
                         v .:? "stack" <*>
                         v .:? "buttons" <*>
                         v .:? "desktop" <*>
                         v .:? "history" <*>
                         v .:? "nonblock" <*>
                         v .:? "reference"
  parseJSON _ = mzero

instance ToJSON PNotify where
  toJSON (PNotify { _title
                  , _title_escape
                  , _text
                  , _text_escape
                  , _styling
                  , _addclass
                  , _cornerclass
                  , _auto_display
                  , _width
                  , _min_height
                  , _type
                  , _icon
                  , _animation
                  , _animate_speed
                  , _position_animate_speed
                  , _opacity
                  , _shadow
                  , _hide
                  , _delay
                  , _mouse_reset
                  , _remove
                  , _insert_brs
                  , _stack
                  , _buttons
                  , _desktop
                  , _history
                  , _nonblock
                  , _reference
                  })
      = object $ maybe [] (\x -> ["title" .= x]) _title ++
                 maybe [] (\x -> ["title_escape" .= x]) _title_escape ++
                 maybe [] (\x -> ["text" .= x]) _text ++
                 maybe [] (\x -> ["text_escape" .= x]) _text_escape ++
                 maybe [] (\x -> ["styling" .= x]) _styling ++
                 maybe [] (\x -> ["addclass" .= x]) _addclass ++
                 maybe [] (\x -> ["cornerclass" .= x]) _cornerclass ++
                 maybe [] (\x -> ["auto_display" .= x]) _auto_display ++
                 maybe [] (\x -> ["width" .= x]) _width ++
                 maybe [] (\x -> ["min_height" .= x]) _min_height ++
                 maybe [] (\x -> ["type" .= x]) _type ++
                 maybe [] (\x -> ["icon" .= x]) _icon ++
                 maybe [] (\x -> ["animation" .= x]) _animation ++
                 maybe [] (\x -> ["animate_speed" .= x]) _animate_speed ++
                 maybe [] (\x -> ["position_animate_speed" .= x]) _position_animate_speed ++
                 maybe [] (\x -> ["opacity" .= x]) _opacity ++
                 maybe [] (\x -> ["shadow" .= x]) _shadow ++
                 maybe [] (\x -> ["hide" .= x]) _hide ++
                 maybe [] (\x -> ["delay" .= x]) _delay ++
                 maybe [] (\x -> ["mouse_reset" .= x]) _mouse_reset ++
                 maybe [] (\x -> ["remove" .= x]) _remove ++
                 maybe [] (\x -> ["insert_brs" .= x]) _insert_brs ++
                 maybe [] (\x -> ["stack" .= x]) _stack ++
                 maybe [] (\x -> ["buttons" .= x]) _buttons ++
                 maybe [] (\x -> ["desktop" .= x]) _desktop ++
                 maybe [] (\x -> ["history" .= x]) _history ++
                 maybe [] (\x -> ["nonblcok" .= x]) _nonblock ++
                 maybe [] (\x -> ["reference" .= x]) _reference ++
                 []

defaultPNotify :: PNotify
defaultPNotify = PNotify
                 { _title                   = Nothing
                 , _title_escape            = Nothing
                 , _text                    = Nothing
                 , _text_escape             = Nothing
                 , _styling                 = Nothing
                 , _addclass                = Nothing
                 , _cornerclass             = Nothing
                 , _auto_display            = Nothing
                 , _width                   = Nothing
                 , _min_height              = Nothing
                 , _type                    = Nothing
                 , _icon                    = Nothing
                 , _animation               = Nothing
                 , _animate_speed           = Nothing
                 , _position_animate_speed  = Nothing
                 , _opacity                 = Nothing
                 , _shadow                  = Nothing
                 , _hide                    = Nothing
                 , _delay                   = Nothing
                 , _mouse_reset             = Nothing
                 , _remove                  = Nothing
                 , _insert_brs              = Nothing
                 , _stack                   = Nothing
                 , _buttons                 = Nothing
                 , _desktop                 = Nothing
                 , _history                 = Nothing
                 , _nonblock                = Nothing
                 , _reference               = Nothing
                 }

instance RawJS [PNotify] where
  rawJS = rawJS . TL.decodeUtf8 . encode

class YesodJquery a => YesodJqueryPnotify a where
  urlJqueryJs :: a -> Prelude.Either (Route a) Text
  urlJqueryJs _ = Prelude.Right "//ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"
  urlJqueryUiCss :: a -> Prelude.Either (Route a) Text
  urlJqueryUiCss _ = Prelude.Right "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/smoothness/jquery-ui.css"
  
  urlPnotifyBrightthemeCss :: a -> Prelude.Either (Route a) Text
  urlPnotifyBrightthemeCss _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.brighttheme.min.css"
  urlPnotifyButtonsJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyButtonsJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.buttons.min.js"
  urlPnotifyButtonsCss :: a -> Prelude.Either (Route a) Text
  urlPnotifyButtonsCss _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.buttons.min.css"
  urlPnotifyCallbacksJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyCallbacksJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.callbacks.min.js"
  urlPnotifyConfirmJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyConfirmJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.confirm.min.js"
  urlPnotifyCoreJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyCoreJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.core.min.js"
  urlPnotifyCoreCss :: a -> Prelude.Either (Route a) Text
  urlPnotifyCoreCss _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.core.min.css"
  urlPnotifyDesktopJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyDesktopJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.desktop.min.js"
  urlPnotifyHistoryJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyHistoryJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.history.min.js"
  urlPnotifyHistoryCss :: a -> Prelude.Either (Route a) Text
  urlPnotifyHistoryCss _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.history.min.css"
  urlPnotifyMobileJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyMobileJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.mobile.min.js"
  urlPnotifyNonblockJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyNonblockJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.nonblock.min.js"
  urlPnotifyPiconCss :: a -> Prelude.Either (Route a) Text
  urlPnotifyPiconCss _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.picon.min.css"
  urlPnotifyReferenceJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyReferenceJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.reference.min.js"
  urlPnotifyTooltipJs :: a -> Prelude.Either (Route a) Text
  urlPnotifyTooltipJs _ = Prelude.Right "//cdn.css.net/libs/pnotify/2.1.0/pnotify.tooltip.min.js"

  urlBootstrap3Js :: a -> Prelude.Either (Route a) Text
  urlBootstrap3Js _ = Prelude.Right "//netdna.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
  urlBootstrap3Css :: a -> Prelude.Either (Route a) Text
  urlBootstrap3Css _ = Prelude.Right "//netdna.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"

  urlBrightThemeCss :: a -> Prelude.Either (Route a) Text
  urlBrightThemeCss _ = Prelude.Right "//cdnjs.cloudflare.com/ajax/libs/pnotify/2.1.0/pnotify.brighttheme.min.css"

  urlFontAwesomeCss :: a -> Prelude.Either (Route a) Text
  urlFontAwesomeCss _ = Prelude.Right "//netdna.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"

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
optionalLoadJsCss y = mapM_ trans . uniqueAsDefault BrightTheme
    where
      uniqueAsDefault def = nub . map (maybe def id . _styling)
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
      addScriptEither $ urlPnotifyCoreJs y
      addStylesheetEither $ urlPnotifyCoreCss y

      addStylesheetEither $ urlPnotifyBrightthemeCss y
      addScriptEither $ urlPnotifyButtonsJs y
      addStylesheetEither $ urlPnotifyButtonsCss y
      addScriptEither $ urlPnotifyCallbacksJs y
      addScriptEither $ urlPnotifyConfirmJs y
      addScriptEither $ urlPnotifyDesktopJs y
      addScriptEither $ urlPnotifyHistoryJs y
      addStylesheetEither $ urlPnotifyHistoryCss y
      addScriptEither $ urlPnotifyMobileJs y
      addScriptEither $ urlPnotifyNonblockJs y
      addStylesheetEither $ urlPnotifyPiconCss y
      addScriptEither $ urlPnotifyReferenceJs y
      addScriptEither $ urlPnotifyTooltipJs y

      optionalLoadJsCss y ps

      toWidget [julius|$(function(){$.each(#{rawJS ps},function(i,v){new PNotify(v)});});|]
