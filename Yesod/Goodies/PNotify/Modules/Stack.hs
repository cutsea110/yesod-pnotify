module Yesod.Goodies.PNotify.Modules.Stack
       ( Stack(..)
       , defaultStack
       )where
import Prelude hiding (Either(..))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (mzero)

import Yesod.Goodies.PNotify.Types
import Yesod.Goodies.PNotify.Types.Instances


data Dir = Up
         | Down
         | Right
         | Left
         deriving (Eq, Ord, Enum)

instance Read Dir where
  readsPrec d r = do
    (v, s') <- lex r
    return $ case v of
      "up" -> (Up, s')
      "down" -> (Down, s')
      "right" -> (Right, s')
      "left" -> (Left, s')
      _ -> error $ "invalid Dir " ++ v

instance Show Dir where
  show Up = "up"
  show Down = "down"
  show Right = "right"
  show Left = "left"

instance FromJSON Dir where
  parseJSON (String v) = return $ read $ T.unpack v
  parseJSON _ = mzero

instance ToJSON Dir where
  toJSON Up = String "up"
  toJSON Down = String "down"
  toJSON Right = String "right"
  toJSON Left = String "left"

data Push = Top
          | Bottom
          deriving (Eq, Ord, Enum)

instance Read Push where
  readsPrec d r = do
    (v, s') <- lex r
    return $ case v of
      "top" -> (Top, s')
      "bottom" -> (Bottom, s')
      _ -> error $ "invalid Push " ++ v

instance Show Push where
  show Top = "top"
  show Bottom = "bottom"

instance FromJSON Push where
  parseJSON (String v) = return $ read $ T.unpack v
  parseJSON _ = mzero

instance ToJSON Push where
  toJSON Top = String "top"
  toJSON Bottom = String "bottom"


data Stack = Stack { _addpos2    :: Maybe Int
                   , _animation  :: Maybe Bool
                   , _dir1       :: Maybe Dir
                   , _dir2       :: Maybe Dir
                   , _firstpos1  :: Maybe Int
                   , _firstpos2  :: Maybe Int
                   , _push       :: Maybe Push
                   , _spacing1   :: Maybe Int
                   , _spacing2   :: Maybe Int
                   , _context    :: Maybe Text
                   }
           deriving (Read, Show, Eq, Ord)

instance FromJSON Stack where
  parseJSON (Object v) = Stack <$>
                         v .:? "addpos2" <*>
                         v .:? "animation" <*>
                         v .:? "dir1" <*>
                         v .:? "dir2" <*>
                         v .:? "firstpos1" <*>
                         v .:? "firstpos2" <*>
                         v .:? "push" <*>
                         v .:? "spacing1" <*>
                         v .:? "spacing2" <*>
                         v .:? "context"

instance ToJSON Stack where
  toJSON (Stack { _addpos2
                , _animation
                , _dir1
                , _dir2
                , _firstpos1
                , _firstpos2
                , _push
                , _spacing1
                , _spacing2
                , _context
                })
      = object $ maybe [] (\x -> ["addpos2" .= x]) _addpos2 ++
                 maybe [] (\x -> ["animation" .= x]) _animation ++
                 maybe [] (\x -> ["dir1" .= x]) _dir1 ++
                 maybe [] (\x -> ["dir2" .= x]) _dir2 ++
                 maybe [] (\x -> ["firstpos1" .= x]) _firstpos1 ++
                 maybe [] (\x -> ["firstpos2" .= x]) _firstpos2 ++
                 maybe [] (\x -> ["push" .= x]) _push ++
                 maybe [] (\x -> ["spacing1" .= x]) _spacing1 ++
                 maybe [] (\x -> ["spacing2" .= x]) _spacing2 ++
                 maybe [] (\x -> ["context" .= x]) _context ++
                 []

defaultStack :: Stack
defaultStack = Stack
               { _addpos2     = Nothing
               , _animation   = Nothing
               , _dir1        = Nothing
               , _dir2        = Nothing
               , _firstpos1   = Nothing
               , _firstpos2   = Nothing
               , _push        = Nothing
               , _spacing1    = Nothing
               , _spacing2    = Nothing
               , _context     = Nothing
               }
