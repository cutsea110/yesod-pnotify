module Yesod.Goodies.PNotify.Types.Instances where

import Control.Monad (mzero)
import Data.Aeson hiding (Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import Yesod.Goodies.PNotify.Types

instance ToJSON (Either Bool Text)  where
  toJSON (Left b) = Bool b
  toJSON (Right t) = String t

instance FromJSON (Either Bool Text) where
  parseJSON (Bool b) = Left <$> parseJSON (Bool b)
  parseJSON (String t) = Right <$> parseJSON (String t)
  parseJSON _ = mzero

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
  parseJSON _ = mzero

instance ToJSON NotifyType where
  toJSON Notice = String "notice"
  toJSON Info = String "info"
  toJSON Success = String "success"
  toJSON Error = String "error"

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
  parseJSON _ = mzero

instance ToJSON NotifyStyling where
  toJSON JqueryUI = String "jqueryui"
  toJSON Bootstrap3 = String "bootstrap3"
  toJSON BrightTheme = String "brighttheme"
  toJSON FontAwesome = String "fontawesome"

instance Read AnimationType where
  readsPrec d r = do
    (v, s') <- lex r
    return $ case v of
      "none" -> (None, s')
      "fade" -> (Fade, s')
      "slide" -> (Slide, s')
      _ -> error $ "invalid AnimationType " ++ v

instance Show AnimationType where
  show None = "none"
  show Fade = "fade"
  show Slide = "slide"

instance FromJSON AnimationType where
  parseJSON (String v) = return $ read $ T.unpack v
  parseJSON _ = mzero

instance ToJSON AnimationType where
  toJSON None = String "none"
  toJSON Fade = String "fade"
  toJSON Slide = String "slide"

instance Read AnimateSpeed where
  readsPrec d r = do
    (v, s') <- lex r
    return $ case v of
      "slow" -> (Slow, s')
      "def" -> (Def, s')
      "normal" -> (Normal, s')
      "fast" -> (Fast, s')
      _ -> error $ "invalid AnimationType " ++ v

instance Show AnimateSpeed where
  show Slow = "slow"
  show Def = "def"
  show Normal = "normal"
  show Fast = "fast"

instance FromJSON AnimateSpeed where
  parseJSON (String v) = return $ read $ T.unpack v
  parseJSON _ = mzero

instance ToJSON AnimateSpeed where
  toJSON Slow = String "slow"
  toJSON Def = String "def"
  toJSON Normal = String "normal"
  toJSON Fast = String "fast"
