module Yesod.Goodies.PNotify.Modules.History
       ( History(..)
       , defaultHistory
       )where

import Data.Aeson
import Data.Text (Text)

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances

data Labels = Labels { _redisplay :: Maybe Text
                     , _all       :: Maybe Text
                     , _last      :: Maybe Text
                     }
              deriving (Read, Show, Eq, Ord)

instance FromJSON Labels where
  parseJSON (Object v) = Labels <$>
                         v .:? "redisplay" <*>
                         v .:? "all" <*>
                         v .:? "last"

instance ToJSON Labels where
  toJSON (Labels { _redisplay
                 , _all
                 , _last
                 })
      = object $ maybe [] (\x -> ["redisplay" .= x]) _redisplay ++
                 maybe [] (\x -> ["all" .= x]) _all ++
                 maybe [] (\x -> ["last" .= x]) _last ++
                 []

data History = History { _history :: Maybe Bool
                       , _menu    :: Maybe Bool
                       , _fixed   :: Maybe Bool
                       , _maxonscreen  :: Maybe Int
                       , _labels       :: Maybe Labels
                       }
               deriving (Read, Show, Eq, Ord)

instance FromJSON History where
  parseJSON (Object v) = History <$>
                         v .:? "history" <*>
                         v .:? "menu" <*>
                         v .:? "fixed" <*>
                         v .:? "maxonscreen" <*>
                         v .:? "labels"

instance ToJSON History where
  toJSON (History { _history
                  , _menu
                  , _fixed
                  , _maxonscreen
                  , _labels
                  })
      = object $ maybe [] (\x -> ["history" .= x]) _history ++
                 maybe [] (\x -> ["menu" .= x]) _menu ++
                 maybe [] (\x -> ["fixed" .= x]) _fixed ++
                 maybe [] (\x -> ["maxonscreen" .= x]) _maxonscreen ++
                 maybe [] (\x -> ["labels" .= x]) _labels ++
                 []

defaultHistory :: History
defaultHistory = History
                 { _history     = Nothing
                 , _menu        = Nothing
                 , _fixed       = Nothing
                 , _maxonscreen = Nothing
                 , _labels      = Nothing
                 }
