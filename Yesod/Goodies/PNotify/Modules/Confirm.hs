module Yesod.Goodies.PNotify.Modules.Confirm
       ( Confirm(..)
       , defaultConfirm
       )where

import Prelude hiding (Either(..))
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances

data Align = Right
           | Left
           | Center
           | Justify
           deriving (Eq, Ord, Enum)

instance Read Align where
  readsPrec d r = do
    (v, s') <- lex r
    return $ case v of
      "right" -> (Right, s')
      "left" -> (Left, s')
      "center" -> (Center, s')
      "justify" -> (Justify, s')
      _ -> error $ "invalid Align " ++ v

instance Show Align where
  show Right = "right"
  show Left = "left"
  show Center = "center"
  show Justify = "justify"

instance FromJSON Align where
  parseJSON (String v) = return $ read $ T.unpack v
  parseJSON _ = mzero

instance ToJSON Align where
  toJSON Right = String "right"
  toJSON Left = String "left"
  toJSON Center = String "center"
  toJSON Justify = String "justify"


data Button = Button { _text :: Maybe Text
                     , _addClass :: Maybe Text
                     , _promptTrigger :: Maybe Bool
                     }
              deriving (Read, Show, Eq, Ord)

instance FromJSON Button where
  parseJSON (Object v) = Button <$>
                         v .:? "text" <*>
                         v .:? "addClass" <*>
                         v .:? "promptTrigger"

instance ToJSON Button where
  toJSON (Button { _text
                 , _addClass
                 , _promptTrigger
                 })
      = object $ maybe [] (\x -> ["text" .= x]) _text ++
                 maybe [] (\x -> ["addClass" .= x]) _addClass ++
                 maybe [] (\x -> ["promptTrigger" .= x]) _promptTrigger ++
                 []

data Confirm = Confirm { _confirm :: Maybe Bool
                       , _prompt :: Maybe Bool
                       , _prompt_class :: Maybe Text
                       , _prompt_default :: Maybe Text
                       , _prompt_multi_line :: Maybe Bool
                       , _align :: Maybe Align
                       , _buttons :: Maybe [Button]
                       }
               deriving (Read, Show, Eq, Ord)

instance FromJSON Confirm where
  parseJSON (Object v) = Confirm <$>
                         v .:? "confirm" <*>
                         v .:? "prompt" <*>
                         v .:? "prompt_class" <*>
                         v .:? "prompt_default" <*>
                         v .:? "prompt_multi_line" <*>
                         v .:? "align" <*>
                         v .:? "buttons"

instance ToJSON Confirm where
  toJSON (Confirm { _confirm
                  , _prompt
                  , _prompt_class
                  , _prompt_default
                  , _prompt_multi_line
                  , _align
                  , _buttons
                  })
      = object $ maybe [] (\x -> ["confirm" .= x]) _confirm ++
                 maybe [] (\x -> ["prompt" .= x]) _prompt ++
                 maybe [] (\x -> ["prompt_class" .= x]) _prompt_class ++
                 maybe [] (\x -> ["prompt_default" .= x]) _prompt_default ++
                 maybe [] (\x -> ["prompt_multi_line" .= x]) _prompt_multi_line ++
                 maybe [] (\x -> ["align" .= x]) _align ++
                 maybe [] (\x -> ["buttons" .= x]) _buttons ++
                 []


defaultConfirm :: Confirm
defaultConfirm = Confirm { _confirm = Nothing
                         , _prompt = Nothing
                         , _prompt_class = Nothing
                         , _prompt_default = Nothing
                         , _prompt_multi_line = Nothing
                         , _align = Nothing
                         , _buttons = Nothing
                         }
