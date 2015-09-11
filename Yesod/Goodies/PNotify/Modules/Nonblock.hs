module Yesod.Goodies.PNotify.Modules.Nonblock
       ( Nonblock(..)
       , defaultNonblock
       )where

import Data.Aeson
import Data.Text (Text)

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances

data Nonblock = Nonblock { _nonblock          :: Maybe Bool
                         , _nonblock_opacity  :: Maybe Double
                         }
                deriving (Read, Show, Eq, Ord)

instance FromJSON Nonblock where
  parseJSON (Object v) = Nonblock <$>
                         v .:? "nonblock" <*>
                         v .:? "nonblock_opacity"

instance ToJSON Nonblock where
  toJSON (Nonblock { _nonblock
                   , _nonblock_opacity
                   })
      = object $ maybe [] (\x -> ["nonblock" .= x]) _nonblock ++
                 maybe [] (\x -> ["nonblock_opacity" .= x]) _nonblock_opacity ++
                 []

defaultNonblock :: Nonblock
defaultNonblock = Nonblock
                  { _nonblock         = Nothing
                  , _nonblock_opacity = Nothing
                  }
