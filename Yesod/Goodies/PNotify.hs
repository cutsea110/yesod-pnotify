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

import Data.Text (Text)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans.Maybe
import Data.Char (toLower)
import Text.Julius (RawJS(..))

data PNotify = PNotify 
               { sty :: NotifyStyling
               , typ :: NotifyType
               , ttl :: Text
               , msg :: Text
               }
             deriving (Show, Read)

data NotifyType = Notice | Info | Success | Error
                deriving (Show, Read)

data NotifyStyling = JqueryUI | Bootstrap3 | BrightTheme
                   deriving (Show, Read)

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

notifyKey :: Text
notifyKey = "_PNotify"

toText :: [PNotify] -> Text
toText = T.concat . TL.toChunks . TL.pack . show

fromText :: Text -> [PNotify]
fromText = read . T.unpack

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

pnotify :: YesodJqueryPnotify site => site -> WidgetT site IO ()
pnotify y = do
  mnotify <- handlerToWidget getPNotify
  case mnotify of
    Nothing -> return ()
    Just ps -> do
      addScriptEither $ urlJqueryJs y
      addScriptEither $ urlPnotifyJs y
      addStylesheetEither $ urlPnotifyCss y
      addScriptEither $ urlBootstrap3Js y
      addStylesheetEither $ urlBootstrap3Css y
      addStylesheetEither $ urlBrightThemeCss y
      addStylesheetEither $ urlJqueryUiCss y
      let toJs p = [julius|{styling:'#{rawJS $ map toLower $ show $ sty p}'
                           ,title:'#{rawJS $ ttl p}'
                           ,text:'#{rawJS $ msg p}'
                           ,type:'#{rawJS $ map toLower $ show $ typ p}'
                           },|]
          ws = foldr ((<>).toJs) mempty ps
      toWidget [julius|$(function(){var ws=[^{ws}];for(var i in ws){new PNotify(ws[i]);}});|]
