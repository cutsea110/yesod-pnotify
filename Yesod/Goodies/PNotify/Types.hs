module Yesod.Goodies.PNotify.Types where

import Data.Text (Text)

type URL = Text

data NotifyType = Notice
                | Info
                | Success
                | Error
                deriving (Eq, Ord, Enum)

data NotifyStyling = JqueryUI
                   | Bootstrap3
                   | BrightTheme
                   | FontAwesome
                   deriving (Eq, Ord, Enum)

data AnimationType = None
                   | Fade
                   | Slide
                   deriving (Eq, Ord, Enum)

data AnimateSpeed = Slow
                  | Def
                  | Normal
                  | Fast
                  deriving (Eq, Ord, Enum)
