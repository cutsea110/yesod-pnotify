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
import Yesod.Form.Jquery

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
                deriving (Read)
instance Show NotifyType where
  show Notice = "notice"
  show Info = "info"
  show Success = "success"
  show Error = "error"

data NotifyStyling = JqueryUI | Bootstrap3 | BrightTheme
                   deriving (Read)
instance Show NotifyStyling where
  show JqueryUI = "jqueryui"
  show Bootstrap3 = "bootstrap3"
  show BrightTheme = "brighttheme"

class YesodJquery a => YesodJqueryPnotify a where
  urlPnotifyJs :: a -> Either (Route a) Text
  urlPnotifyJs _ = Right "http://cdn.jsdelivr.net/pnotify/2.1.0/pnotify.all.min.js"
  urlPnotifyCss :: a -> Either (Route a) Text
  urlPnotifyCss _ = Right "http://cdn.jsdelivr.net/pnotify/2.1.0/pnotify.all.min.css"

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
      let toJs p = [julius|{styling:'#{rawJS $ show $ sty p}'
                           ,title:'#{rawJS $ ttl p}'
                           ,text:'#{rawJS $ msg p}'
                           ,type:'#{rawJS $ show $ typ p}'
                           },|]
          ws = foldr ((<>).toJs) mempty ps
      toWidget [julius|$(function(){var ws=[^{ws}];for(var i in ws){new Pnotify(ws[i]);}});|]
