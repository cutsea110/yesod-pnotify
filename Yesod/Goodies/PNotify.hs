{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
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
import qualified Text.Blaze.Html.Renderer.Text as RT
import Control.Monad.Trans.Maybe
import Data.Char (toLower)
import Text.Julius (RawJS(..))

data PNotify = PNotify 
               { sty :: NotifyStyling
               , typ :: NotifyType
               , ttl :: Html
               , msg :: Html
               }

data PNotify_ = PNotify_
                { _sty :: NotifyStyling
                , _typ :: NotifyType
                , _ttl :: Text
                , _msg :: Text
                }
              deriving (Show, Read)

toInternal (PNotify s tp t m) =
    PNotify_ { _sty = s
             , _typ = tp
             , _ttl = T.concat . TL.toChunks . RT.renderHtml $ t
             , _msg = T.concat . TL.toChunks . RT.renderHtml $ m}

fromInternal (PNotify_ s tp t m) =
    PNotify { sty = s
            , typ = tp
            , ttl = preEscapedToMarkup t
            , msg = preEscapedToMarkup m}

data NotifyType = Notice | Info | Success | Error
                deriving (Show, Read)

data NotifyStyling = JqueryUI | Bootstrap3
                   deriving (Show, Read)


class YesodJquery a => YesodJqueryPnotify a where
  urlPnotifyJs :: a -> Either (Route a) Text
  urlPnotifyJs _ = Right "http://cdn.jsdelivr.net/pnotify/2.0.0/pnotify.all.min.js"
  urlPnotifyCss :: a -> Either (Route a) Text
  urlPnotifyCss _ = Right "http://cdn.jsdelivr.net/pnotify/2.0.0/pnotify.all.min.css"
  urlPnotifyIconsCss :: a -> Either (Route a) Text
  urlPnotifyIconsCss _ = Right "http://cdn.jsdelivr.net/pnotify/2.0.0/pnotify.picon.css"

notifyKey :: Text
notifyKey = "_PNotify"

_setPNotify :: [PNotify] -> HandlerT site IO ()
_setPNotify ps = setSession notifyKey . T.concat . TL.toChunks . TL.pack . show $ map toInternal ps

_getPNotify :: HandlerT site IO (Maybe [PNotify_])
_getPNotify = runMaybeT $ do
  ns <- MaybeT $ lookupSession notifyKey
  lift $ deleteSession notifyKey
  return $ read $ T.unpack ns

getPNotify :: HandlerT site IO (Maybe [PNotify])
getPNotify = runMaybeT $ do
  ps <- MaybeT _getPNotify
  return $ map fromInternal ps

setPNotify :: PNotify -> HandlerT site IO ()
setPNotify n = do
  mns <- getPNotify
  _setPNotify (n:maybe [] id mns)

pnotify :: YesodJqueryPnotify site => site -> WidgetT site IO ()
pnotify y = do
  mnotify <- handlerToWidget _getPNotify
  case mnotify of
    Nothing -> return ()
    Just _ps -> do
      addScriptEither $ urlJqueryJs y
      addScriptEither $ urlJqueryUiJs y
      addStylesheetEither $ urlJqueryUiCss y
      addScriptEither $ urlPnotifyJs y
      addStylesheetEither $ urlPnotifyCss y
      addStylesheetEither $ urlPnotifyIconsCss y
      let toJs p = [julius|{styling:'#{rawJS $ map toLower $ show $ _sty p}',title:'#{rawJS $ _ttl p}',text:'#{rawJS $ _msg p}',type:'#{rawJS $ map toLower $ show $ _typ p}'},|]
          ws = foldr ((<>).toJs) mempty _ps
      toWidget [julius|$(document).ready(function(e){var ws=[^{ws}];for(var i in ws){new PNotify(ws[i]);}});|]
