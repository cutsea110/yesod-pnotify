module Yesod.Goodies.PNotify.Modules.Desktop
       ( Desktop(..)
       , defaultDesktop
       )where

import Data.Aeson
import Data.Text (Text)

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances

data Desktop = Desktop { _desktop   :: Maybe Bool
                       , _fallback  :: Maybe Bool
                       , _icon      :: Maybe (Prelude.Either Bool URL)
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
  toJSON (Desktop { _desktop
                  , _fallback
                  , _icon
                  , _tag
                  })
      = object $ maybe [] (\x -> ["desktop" .= x]) _desktop ++
                 maybe [] (\x -> ["fallback" .= x]) _fallback ++
                 maybe [] (\x -> ["icon" .= x]) _icon ++
                 maybe [] (\x -> ["tag" .= x]) _tag ++
                 []

defaultDesktop :: Desktop
defaultDesktop = Desktop
                 { _desktop  = Nothing
                 , _fallback = Nothing
                 , _icon     = Nothing
                 , _tag      = Nothing

                 }
