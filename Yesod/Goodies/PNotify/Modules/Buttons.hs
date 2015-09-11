module Yesod.Goodies.PNotify.Modules.Buttons where

import Data.Aeson
import Data.Text (Text)

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances

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


defaultButtons :: Buttons
defaultButtons = Buttons
                 { _closer             = Nothing
                 , _closer_hover       = Nothing
                 , _sticker            = Nothing
                 , _sticker_hover      = Nothing
                 , _show_on_nonblock   = Nothing
                 , _labels             = Nothing
                 }
