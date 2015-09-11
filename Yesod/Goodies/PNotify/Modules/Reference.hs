module Yesod.Goodies.PNotify.Modules.Reference
       ( Reference(..)
       , defaultReference
       )where

import Data.Aeson
import Data.Text (Text)

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances

data Labels = Labels { _text :: Maybe Text
                     }
              deriving (Read, Show, Eq, Ord)

instance FromJSON Labels where
  parseJSON (Object v) = Labels <$>
                         v .:? "text"

instance ToJSON Labels where
  toJSON (Labels { _text
                 })
      = object $ maybe [] (\x -> ["text" .= x]) _text ++
                 []

data Reference = Reference { _putThing :: Maybe Bool
                           , _labels   :: Maybe Text
                           }
                 deriving (Read, Show, Eq, Ord)

instance FromJSON Reference where
  parseJSON (Object v) = Reference <$>
                         v .:? "putThing" <*>
                         v .:? "labels"

instance ToJSON Reference where
  toJSON (Reference { _putThing
                    , _labels
                    })
      = object $ maybe [] (\x -> ["putThing" .= x]) _putThing ++
                 maybe [] (\x -> ["labels" .= x]) _labels ++
                 []

defaultReference :: Reference
defaultReference = Reference
                   { _putThing = Nothing
                   , _labels = Nothing
                   }
