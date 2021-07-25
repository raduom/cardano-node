module Cardano.Logging.Test.Messages (
    Message(..)
  , namesForMessage
  , severityForMessage
  , privacyForMessage
  , docMessage
  , ScriptedMessage(..)
  , Script(..)
  ) where

import           Data.Aeson (Value (..), (.=))
import           Data.Text
import           Test.QuickCheck

import           Cardano.Logging

data Message =
    Message1 Int
  | Message2 Text
  | Message3 Double
  deriving (Eq, Ord, Show)

showT :: Show a => a -> Text
showT = pack . show

instance LogFormatting Message where
  forMachine _dtal (Message1 i) =
    mkObject [ "kind" .= String "Message1"
             , "workload" .= String (showT i)
             ]
  forMachine DBrief (Message2 _s) =
    mkObject [ "kind" .= String "Message2"
             ]
  forMachine _dtal (Message2 s) =
    mkObject [ "kind" .= String "Message2"
             , "workload" .= String s
             ]
  forMachine _dtal (Message3 d) =
    mkObject [ "kind" .= String "Message3"
             , "workload" .= String (showT d)
             ]
  forHuman (Message1 i) =
      "Message1 " <> showT i
  forHuman (Message2 s) =
      "Message2 " <> s
  forHuman (Message3 d) =
      "Message3 " <> showT d
  asMetrics (Message1 i) =
      [IntM ["Metrics1"] (fromIntegral i)]
  asMetrics _ = []    

instance Arbitrary Message where
  arbitrary = oneof
    [ Message1 <$> arbitrary,
      Message2 <$> elements ["Hallo", "Goodbye", "Whatelse"],
      Message3 <$> arbitrary
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
      (Message1 1)
      []
      "The first message."
  , DocMsg
      (Message2 "")
      []
      "The second message."
  , DocMsg
      (Message3 1.0)
      [(["Metrics1"], "Oh yeah")]
      "The third message."
  ]

data ScriptedMessage = ScriptedMessage Double Message
  deriving (Eq, Show)

instance Ord ScriptedMessage where
  compare (ScriptedMessage d1 _m1) (ScriptedMessage d2 _m2) = compare d1 d2

instance Arbitrary ScriptedMessage where
  arbitrary = ScriptedMessage <$> choose (0.0,10000.0) <*> arbitrary

newtype Script = Script [ScriptedMessage]
  deriving (Eq, Show)

instance Arbitrary Script where
  arbitrary = Script <$> listOf arbitrary
