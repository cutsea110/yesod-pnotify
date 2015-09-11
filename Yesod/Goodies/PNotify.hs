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

import Yesod.Goodies.PNotify.Types hiding (_animation)
import Yesod.Goodies.PNotify.Types.Instances

data Buttons = Buttons { _closer            :: Maybe Bool
                       , _closer_hover      :: Maybe Bool
                       , _sticker           :: Maybe Bool
                       , _sticker_hover     :: Maybe Bool
                       , _show_on_nonblock  :: Maybe Bool
                       , _labels            :: Maybe Labels
                       }
               deriving (Read, Show, Eq, Ord)

instance FromJSON Buttons where
  parseJSON (Object v) = Buttons <$>
                         v .:? "closer" <*>
                         v .:? "closer_hover" <*>
                         v .:? "sticker" <*>
                         v .:? "sticker_hover" <*>
                         v .:? "show_on_nonblock" <*>
                         v .:? "labels"

instance ToJSON Buttons where
  toJSON (Buttons { _closer
                  , _closer_hover
                  , _sticker
                  , _sticker_hover
                  , _show_on_nonblock
                  , _labels
                  })
      = object $ maybe [] (\x -> ["closer" .= x]) _closer ++
                 maybe [] (\x -> ["closer_hover" .= x]) _closer_hover ++
                 maybe [] (\x -> ["sticker" .= x]) _sticker ++
                 maybe [] (\x -> ["sticker_hover" .= x]) _sticker_hover ++
                 maybe [] (\x -> ["show_on_nonblock" .= x]) _show_on_nonblock ++
                 maybe [] (\x -> ["labels" .= x]) _labels ++
                 []

data Labels = Labels { _close :: Maybe Text
                     , _stick :: Maybe Text
                     }
              deriving (Read, Show, Eq, Ord)

instance FromJSON Labels where
  parseJSON (Object v) = Labels <$>
                         v .:? "close" <*>
                         v .:? "stick"

instance ToJSON Labels where
  toJSON (Labels { _close
                 , _stick
                 })
      = object $ maybe [] (\x -> ["close" .= x]) _close ++
                 maybe [] (\x -> ["stick" .= x]) _stick ++
                 []

type URL = Text

data Desktop = Desktop { _desktop'  :: Maybe Bool
                       , _fallback  :: Maybe Bool
                       , _icon'     :: Maybe (Prelude.Either Bool URL)
                       , _tag       :: Maybe Text
                       }
               deriving (Read, Show, Eq, Ord)

instance FromJSON Desktop where
  parseJSON (Object v) = Desktop <$>
                         v .:? "desktop" <*>
                         v .:? "fallback" <*>
                         v .:? "icon" <*>
                         v .:? "tag"

instance ToJSON Desktop where
  toJSON (Desktop { _desktop'
                  , _fallback
                  , _icon'
                  , _tag
                  })
      = object $ maybe [] (\x -> ["desktop" .= x]) _desktop' ++
                 maybe [] (\x -> ["fallback" .= x]) _fallback ++
                 maybe [] (\x -> ["icon" .= x]) _icon' ++
                 maybe [] (\x -> ["tag" .= x]) _tag ++
                 []

data Stack = Stack { _addpos2    :: Maybe Int
                   , _animation' :: Maybe Bool
                   , _dir1       :: Maybe Dir
                   , _dir2       :: Maybe Dir
                   , _firstpos1  :: Maybe Int
                   , _firstpos2  :: Maybe Int
                   , _push       :: Maybe Push
                   , _spacing1   :: Maybe Int
                   , _spacing2   :: Maybe Int
                   , _context    :: Maybe Text
                   }
           deriving (Read, Show, Eq, Ord)

instance FromJSON Stack where
  parseJSON (Object v) = Stack <$>
                         v .:? "addpos2" <*>
                         v .:? "animation" <*>
                         v .:? "dir1" <*>
                         v .:? "dir2" <*>
                         v .:? "firstpos1" <*>
                         v .:? "firstpos2" <*>
                         v .:? "push" <*>
                         v .:? "spacing1" <*>
                         v .:? "spacing2" <*>
                         v .:? "context"

instance ToJSON Stack where
  toJSON (Stack { _addpos2
                , _animation'
                , _dir1
                , _dir2
                , _firstpos1
                , _firstpos2
                , _push
                , _spacing1
                , _spacing2
                , _context
                })
      = object $ maybe [] (\x -> ["addpos2" .= x]) _addpos2 ++
                 maybe [] (\x -> ["animation" .= x]) _animation' ++
                 maybe [] (\x -> ["dir1" .= x]) _dir1 ++
                 maybe [] (\x -> ["dir2" .= x]) _dir2 ++
                 maybe [] (\x -> ["firstpos1" .= x]) _firstpos1 ++
                 maybe [] (\x -> ["firstpos2" .= x]) _firstpos2 ++
                 maybe [] (\x -> ["push" .= x]) _push ++
                 maybe [] (\x -> ["spacing1" .= x]) _spacing1 ++
                 maybe [] (\x -> ["spacing2" .= x]) _spacing2 ++
                 maybe [] (\x -> ["context" .= x]) _context ++
                 []

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

               , _stack                    :: Maybe Stack
               , _buttons                  :: Maybe Buttons
               , _desktop                  :: Maybe Desktop
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
                         v .:? "desktop"
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
                 }

defaultStack :: Stack
defaultStack = Stack
               { _addpos2     = Nothing
               , _animation'  = Nothing
               , _dir1        = Nothing
               , _dir2        = Nothing
               , _firstpos1   = Nothing
               , _firstpos2   = Nothing
               , _push        = Nothing
               , _spacing1    = Nothing
               , _spacing2    = Nothing
               , _context     = Nothing
               }

defaultButtons :: Buttons
defaultButtons = Buttons
                 { _closer             = Nothing
                 , _closer_hover       = Nothing
                 , _sticker            = Nothing
                 , _sticker_hover      = Nothing
                 , _show_on_nonblock   = Nothing
                 , _labels             = Nothing
                 }
                 
defaultDesktop :: Desktop
defaultDesktop = Desktop
                 { _desktop' = Nothing
                 , _fallback = Nothing
                 , _icon'    = Nothing
                 , _tag      = Nothing

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
