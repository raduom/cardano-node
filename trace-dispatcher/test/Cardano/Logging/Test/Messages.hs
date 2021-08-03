module Cardano.Logging.Test.Messages (
    Message(..)
  , MessageID
  , ScriptedMessage(..)
  , Script(..)
  , namesForMessage
  , severityForMessage
  , privacyForMessage
  , docMessage
  , setMessageID
  , getMessageID
  ) where

import           Data.Aeson (Value (..), (.=))
import           Data.Text
import           Test.QuickCheck

import           Cardano.Logging

-- | Adds a time between 0 and 1.
--   0 is the time of the test start, and 1 the test end
data ScriptedMessage = ScriptedMessage Double Message
  deriving (Eq, Show)

-- Ordered by time
instance Ord ScriptedMessage where
  compare (ScriptedMessage d1 _m1) (ScriptedMessage d2 _m2) = compare d1 d2

instance Arbitrary ScriptedMessage where
  arbitrary = ScriptedMessage <$> choose (0.0, 1.0) <*> arbitrary

newtype Script = Script [ScriptedMessage]
  deriving (Eq, Show)

instance Arbitrary Script where
  arbitrary = Script <$> listOf arbitrary

type MessageID = Int

data Message =
    Message1 MessageID Int
  | Message2 MessageID Text
  | Message3 MessageID Double
  deriving (Eq, Ord, Show)

getMessageID :: Message -> MessageID
getMessageID (Message1 mid _) = mid
getMessageID (Message2 mid _) = mid
getMessageID (Message3 mid _) = mid

setMessageID :: Message -> MessageID -> Message
setMessageID (Message1 _ v) mid = Message1 mid v
setMessageID (Message2 _ v) mid = Message2 mid v
setMessageID (Message3 _ v) mid = Message3 mid v

showT :: Show a => a -> Text
showT = pack . show

instance LogFormatting Message where
  forMachine _dtal (Message1 mid i) =
    mkObject [ "kind" .= String "Message1"
             , "mid" .= ("<" <> showT mid <> ">")
             , "workload" .= String (showT i)
             ]
  forMachine DMinimal (Message2 mid _s) =
    mkObject [ "mid" .= ("<" <> showT mid <> ">")
             , "kind" .= String "Message2"
             ]
  forMachine _dtal (Message2 mid s) =
    mkObject [ "kind" .= String "Message2"
             , "mid" .= String ("<" <> showT mid <> ">")
             , "workload" .= String s
             ]
  forMachine _dtal (Message3 mid d) =
    mkObject [ "kind" .= String "Message3"
             , "mid" .= String (showT mid)
             , "workload" .= String (showT d)
             ]
  forHuman (Message1 mid i) =
      "Message1 <" <> showT mid <> "> " <> showT i
  forHuman (Message2 mid s) =
      "Message2 <" <> showT mid <> "> " <> s
  forHuman (Message3 mid d) =
      "Message3 <" <> showT mid <> "> " <> showT d
  asMetrics (Message1 mid _i) =
      [IntM ["Metrics1"] (fromIntegral mid)]
  asMetrics _ = []

instance Arbitrary Message where
  arbitrary = oneof
    [ Message1 0 <$> arbitrary,
      Message2 0 <$> elements ["Hallo", "Goodbye", "Whatelse"],
      Message3 0 <$> arbitrary
    ]

namesForMessage :: Message -> [Text]
namesForMessage Message1 {} = ["Message1"]
namesForMessage Message2 {} = ["Message2"]
namesForMessage Message3 {} = ["Message3"]

severityForMessage :: Message -> SeverityS
severityForMessage Message1 {} = Debug
severityForMessage Message2 {} = Info
severityForMessage Message3 {} = Error

privacyForMessage :: Message -> Privacy
privacyForMessage Message1 {} = Public
privacyForMessage Message2 {} = Confidential
privacyForMessage Message3 {} = Public

docMessage :: Documented Message
docMessage = Documented [
    DocMsg
      (Message1 1 1)
      []
      "The first message."
  , DocMsg
      (Message2 1 "")
      []
      "The second message."
  , DocMsg
      (Message3 1 1.0)
      [(["Metrics1"], "A number")]
      "The third message."
  ]
