{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Oracles (
    oracleFiltering
  ) where

import qualified Data.Text as T
import           Test.QuickCheck
import           Text.Read (readMaybe)

import           Cardano.Logging
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Types

oracleFiltering ::  TraceConfig -> ScriptRes -> Property
oracleFiltering conf ScriptRes {..} =
    let Script msgs = srScript
    in property $ and (map oracleMessage msgs)
  where
    oracleMessage :: ScriptedMessage -> Bool
    oracleMessage (ScriptedMessage _t msg) =
      let filterSeverity = getSeverity conf ("Node" : namesForMessage msg)
          backends = getBackends conf ("Node" : namesForMessage msg)
          inStdout = hasStdoutBackend backends
                      && fromEnum (severityForMessage msg) >= fromEnum filterSeverity
          isCorrectStdout = checkInclusion msg srStdoutRes == inStdout
          inForwarder = elem Forwarder backends
                      && fromEnum (severityForMessage msg) >= fromEnum filterSeverity
                      && privacyForMessage msg == Public
          isCorrectForwarder = checkInclusion msg srForwardRes == inForwarder
          inEKG = elem EKGBackend backends
                      && fromEnum (severityForMessage msg) >= fromEnum filterSeverity
          isCorrectEKG = checkInclusion msg srEkgRes == inEKG
      in isCorrectStdout && isCorrectForwarder && isCorrectEKG

hasStdoutBackend :: [BackendConfig] -> Bool
hasStdoutBackend []             = False
hasStdoutBackend (Stdout _ : _) = True
hasStdoutBackend (_ : rest)     = hasStdoutBackend rest

checkInclusion :: Message -> [FormattedMessage] -> Bool
checkInclusion msg list =
    let msgID = getMessageID msg
    in case occurences msgID list of
          1 -> True
          0 -> False
          _ -> error $ "Multiple occurences of message " <> show msgID

occurences :: MessageID -> [FormattedMessage] -> Int
occurences _mid [] = 0
occurences  mid (fmsg : rest) = if identifyMsg mid fmsg
                                  then 1 + occurences mid rest
                                  else occurences mid rest

identifyMsg :: MessageID -> FormattedMessage -> Bool
identifyMsg mid (FormattedMetrics [IntM _ idm])
                                        = fromIntegral idm == mid
identifyMsg _   (FormattedMetrics [])   = False
identifyMsg mid (FormattedHuman _ txt)  = idInText txt mid
identifyMsg mid (FormattedMachine txt)  = idInText txt mid
identifyMsg mid (FormattedForwarder to) =
  case toHuman to of
    Just txt -> idInText txt mid
    Nothing  -> case toMachine to of
                  Just txt -> idInText txt mid
                  Nothing  -> error "No text found in trace object"

idInText :: T.Text -> MessageID -> Bool
idInText txt mid =
  let ntxt = T.takeWhile (\c -> c /= '>')
                (T.dropWhile (\c -> c /= '<') txt)
  in case readMaybe (T.unpack ntxt) :: Maybe Int of
        Nothing -> False
        Just i  -> i == mid
