
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


module Cardano.PlutusExample.ScriptContextChecker where

import           Prelude hiding (($))

import           Cardano.Api
import           Cardano.Api.Byron
import           Cardano.Api.Shelley
import qualified Cardano.Api.Shelley as Api

import           Codec.Serialise
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.Trans.Except.Extra
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import           Data.Maybe as M
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           System.FilePath.Posix

import           Cardano.CLI.Environment
import           Cardano.CLI.Shelley.Output
import           Cardano.CLI.Shelley.Run.Query
import           Cardano.CLI.Types (SocketPath (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Slotting.EpochInfo (EpochInfo)
import           Cardano.Slotting.Time (SystemStart)
import qualified Ledger as Plutus
import qualified Ledger.Typed.Scripts as Scripts
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import qualified Plutus.V1.Ledger.DCert as Plutus
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AMap
import           PlutusTx.IsData.Class
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.Prelude as P
import qualified Shelley.Spec.Ledger.TxBody as Shelley

newtype MyCustomDatum = MyCustomDatum Integer

-- MyCustomRedeemer mimics the 'ScriptContext' in the Plutus validator.
-- Why? We construct a 'MyCustomRedeemer' from the transaction about
-- to be submitted. We then equivalence test various fields in the
-- Plutus script validator to be sure the correct values are being
-- passed to the Plutus script.

data MyCustomRedeemer
  = MyCustomRedeemer
      { mCrOutputs :: [Plutus.TxOut]
      , mCrInputs :: [Plutus.TxInInfo]
      , mCrMint :: Plutus.Value
      , mCrValidRange :: Plutus.POSIXTimeRange
      , mCrFee :: Plutus.Value
      , mCrDatums :: [(Plutus.DatumHash, Plutus.Datum)]
      , mCrCerts :: [Plutus.DCert]
      , mCrSignatories :: [Plutus.PubKeyHash]
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''MyCustomDatum
PlutusTx.unstableMakeIsData ''MyCustomRedeemer

data ScriptContextTest
instance Scripts.ValidatorTypes ScriptContextTest where
    type instance DatumType ScriptContextTest    = MyCustomDatum
    type instance RedeemerType ScriptContextTest = MyCustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum-> MyCustomRedeemer -> Plutus.ScriptContext -> Bool
mkValidator _datum (MyCustomRedeemer txouts txins minted txValidRange _fee datumsAndHashes certs signatories) scriptContext =
  -- Minted field is equivalent
  Plutus.txInfoMint txInfo P.== minted P.&&
  -- Validity range is equivalent
  Plutus.txInfoValidRange txInfo P.== txValidRange P.&&
  -- Datums and datum hashes are equivalent
  Plutus.txInfoData txInfo P.== datumsAndHashes P.&&
  -- Required tx signers are equivalent
  Plutus.txInfoSignatories txInfo P.== signatories P.&&
  -- Payment tx out is equivalent
  AMap.member paymentOutputFromRedeemer scriptContextOutputsMap P.&&
  -- Txins are equivalent
  (AMap.member txinA scriptContextTxinsMap P.&& AMap.member txinB scriptContextTxinsMap) P.&&
  -- Cert if equivalent
  AMap.member singleRedeemerCert scriptContextCertsMap
 where
   scriptContextCertsMap :: AMap.Map Plutus.DCert Integer
   scriptContextCertsMap = AMap.fromList P.$ P.zip (Plutus.txInfoDCert txInfo) [1]

   singleRedeemerCert :: Plutus.DCert
   singleRedeemerCert = P.head certs

   txinA :: Plutus.TxInInfo
   txinA = P.head redeemerTxins

   txinB :: Plutus.TxInInfo
   txinB = P.head $ P.reverse redeemerTxins

   redeemerTxins :: [Plutus.TxInInfo]
   redeemerTxins = txins

   scriptContextTxins :: [Plutus.TxInInfo]
   scriptContextTxins = Plutus.txInfoInputs txInfo

   scriptContextTxinsMap :: AMap.Map Plutus.TxInInfo Integer
   scriptContextTxinsMap = AMap.fromList P.$ P.zip scriptContextTxins [1,2 :: Integer]

   -- This is paid to the dummy address. We can't compute the change address amount
   -- because the redeemer we computed is based on an older tx which affects the fee
   -- and therefore the change address amount.
   paymentOutputFromRedeemer :: Plutus.Value
   paymentOutputFromRedeemer = P.head $ P.reverse redeemerValues

   redeemerValues :: [Plutus.Value]
   redeemerValues = P.map Plutus.txOutValue txouts

   scriptContextOutputValues :: [Plutus.Value]
   scriptContextOutputValues = P.map Plutus.txOutValue $ Plutus.txInfoOutputs txInfo

   scriptContextOutputsMap :: AMap.Map Plutus.Value Integer
   scriptContextOutputsMap = AMap.fromList P.$ P.zip scriptContextOutputValues [1,2 :: Integer]

   txInfo :: Plutus.TxInfo
   txInfo = Plutus.scriptContextTxInfo scriptContext

inst :: Scripts.TypedValidator ScriptContextTest
inst = Scripts.mkTypedValidator @ScriptContextTest
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MyCustomDatum @MyCustomRedeemer

validator :: Plutus.Validator
validator = Scripts.validatorScript inst

script :: Plutus.Script
script = Plutus.unValidatorScript validator

scriptContextCheckAsShortBs :: SBS.ShortByteString
scriptContextCheckAsShortBs = SBS.toShort . LB.toStrict $ serialise script

scriptContextCheckScript :: PlutusScript PlutusScriptV1
scriptContextCheckScript = PlutusScriptSerialised scriptContextCheckAsShortBs

------------------------------------------------------

sampleTestScriptContextDataJSON :: LB.ByteString
sampleTestScriptContextDataJSON =
  Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . customRedeemerToScriptData
    $ MyCustomRedeemer
        dummyTxOuts
        dummyTxIns
        dummyLedgerVal
        dummyPOSIXTimeRange
        dummyLedgerVal
        dummyDatumHashes
        dummyCerts
        dummySignatories

customRedeemerToScriptData :: MyCustomRedeemer -> ScriptData
customRedeemerToScriptData cRedeem =
  fromPlutusData $ PlutusTx.builtinDataToData $ toBuiltinData cRedeem

customRedeemerFromScriptData :: ScriptData -> Either String MyCustomRedeemer
customRedeemerFromScriptData sDat =
  let bIData = PlutusTx.dataToBuiltinData $ toPlutusData sDat
  in case fromBuiltinData bIData of
      Just mCRedeem -> Right mCRedeem
      Nothing -> Left "Could not decode MyCustomRedeemer from ScriptData"

dummyCerts :: [Plutus.DCert]
dummyCerts = []

dummyTxIns :: [Plutus.TxInInfo]
dummyTxIns = []

dummySignatories :: [Plutus.PubKeyHash]
dummySignatories = []

dummyDatumHashes :: [(Plutus.DatumHash, Plutus.Datum)]
dummyDatumHashes = []

dummyLedgerVal :: Plutus.Value
dummyLedgerVal = Alonzo.transValue $ toMaryValue Prelude.mempty

dummyTxOuts :: [Plutus.TxOut]
dummyTxOuts = []

dummyPOSIXTimeRange :: Plutus.POSIXTimeRange
dummyPOSIXTimeRange = Plutus.from $ Plutus.POSIXTime 42

data ScriptContextError = NoScriptsInByronEra
                        | NoScriptsInEra
                        | ReadTxBodyError (FileError TextEnvelopeError)
                        | IntervalConvError TransactionValidityIntervalError
                        | AcquireFail AcquireFailure
                        | NoTipLocalStateError
                        | NoSystemStartTimeError
                        | EnvVarSocketErr EnvSocketError
                        | ScriptContextErrorByronEra
                        | QueryError ShelleyQueryCmdError
                        deriving Show

txToCustomRedeemer
  :: UTxO era
  -> EpochInfo (Either TransactionValidityIntervalError)
  -> SystemStart
  -> Api.Tx era
  -> Either ScriptContextError MyCustomRedeemer
txToCustomRedeemer _ _ _ (ByronTx _) = Left NoScriptsInByronEra
txToCustomRedeemer utxo eInfo sStart (ShelleyTx ShelleyBasedEraAlonzo ledgerTx) = do
  let txBody = Alonzo.body ledgerTx
      witness = Alonzo.wits ledgerTx
      mTxIns = Prelude.map (Alonzo.txInfoIn (toLedgerUTxO ShelleyBasedEraAlonzo utxo)) . Set.toList $ Alonzo.inputs txBody
      mTouts = Prelude.map Alonzo.txInfoOut $ seqToList $ Alonzo.outputs txBody
      minted = Alonzo.transValue $ Alonzo.mint txBody
      txfee = Alonzo.transValue . toMaryValue . lovelaceToValue . fromShelleyLovelace $ Alonzo.txfee txBody
      Alonzo.TxDats datumHashMap = Alonzo.txdats witness
      datumHashes = Prelude.map Alonzo.transDataPair $ Map.toList datumHashMap
      _txid = Alonzo.txInfoId . toShelleyTxId $ getTxIdShelley ShelleyBasedEraAlonzo txBody
      txcerts = Prelude.map Alonzo.transDCert . seqToList $ Alonzo.txcerts txBody
      txsignatories = Prelude.map Alonzo.transKeyHash . Set.toList $ Alonzo.reqSignerHashes txBody
  valRange <-
    first IntervalConvError
      $ Alonzo.transVITime eInfo sStart $ Alonzo.txvldt txBody

  tOuts <- if Prelude.all M.isJust mTouts
           then return $ catMaybes mTouts
           else Prelude.error "Tx Outs not all Just"
  txins <- if Prelude.all M.isJust mTxIns
           then return $ catMaybes mTxIns
           else Prelude.error "Tx Ins not all Just"
  Right $ MyCustomRedeemer tOuts txins minted valRange txfee datumHashes txcerts txsignatories
 where
  seqToList (x Seq.:<| rest) = x : seqToList rest
  seqToList Seq.Empty = []

txToCustomRedeemer _ _ _ (ShelleyTx _ _) = Left NoScriptsInEra

testScriptContextToScriptData :: MyCustomRedeemer -> ScriptData
testScriptContextToScriptData = fromPlutusData . PlutusTx.builtinDataToData . toBuiltinData

readCustomRedeemerFromTx
  :: FilePath
  -> AnyConsensusModeParams
  -> NetworkId
  -> ExceptT ScriptContextError IO MyCustomRedeemer
readCustomRedeemerFromTx fp (AnyConsensusModeParams cModeParams) network = do
  InAnyCardanoEra cEra alonzoTx
    <- firstExceptT ReadTxBodyError
         . newExceptT
         $ readFileTextEnvelopeAnyOf
             [ FromSomeType (AsTx AsAlonzoEra) (InAnyCardanoEra AlonzoEra)
             ]
             fp

  sbe <- getSbe $ cardanoEraStyle cEra
  SocketPath sockPath <- firstExceptT EnvVarSocketErr readEnvSocketPath
  case consensusModeOnly cModeParams of
    CardanoMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

      (_chainTip, eQueryTipLocalState) <- liftIO $ queryQueryTip localNodeConnInfo Nothing
      qTipLocalState <- firstExceptT AcquireFail $ hoistEither eQueryTipLocalState
      let eInfo = epochInfo qTipLocalState
      sStart <- hoistMaybe NoSystemStartTimeError $ mSystemStart qTipLocalState

      -- Query UTxO
      let utxoQ = QueryInShelleyBasedEra sbe (QueryUTxO QueryUTxOWhole)
          utxoQinMode = case toEraInMode cEra CardanoMode of
                          Just eInMode -> QueryInEra eInMode utxoQ
                          Nothing -> Prelude.error "Cannot determine era in mode"
      utxo <- firstExceptT QueryError
                $ executeQuery
                    cEra
                    cModeParams
                    localNodeConnInfo
                    utxoQinMode
      hoistEither $ txToCustomRedeemer utxo eInfo sStart alonzoTx
    _ -> Prelude.error "Please specify --cardano-mode on cli."

txToRedeemer
  :: FilePath
  -> AnyConsensusModeParams
  -> NetworkId
  -> ExceptT ScriptContextError IO ()
txToRedeemer txFp anyCmodeParams nid = do
  testScrContext <- readCustomRedeemerFromTx txFp anyCmodeParams nid
  let redeemer = Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema
                   $ testScriptContextToScriptData testScrContext
      outFp = dropFileName txFp </> "script-context.redeemer"
  liftIO . print $ "Tx generated redeemer: " <> show redeemer
  liftIO $ LB.writeFile outFp redeemer

getSbe :: CardanoEraStyle era -> ExceptT ScriptContextError IO (ShelleyBasedEra era)
getSbe LegacyByronEra = left ScriptContextErrorByronEra
getSbe (ShelleyBasedEra sbe) = return sbe


-- Used in roundtrip testing

fromPlutusTxId :: Plutus.TxId -> Shelley.TxId StandardCrypto
fromPlutusTxId (Plutus.TxId builtInBs) =
  case deserialiseFromRawBytes AsTxId $ fromBuiltin builtInBs of
    Just txidHash -> toShelleyTxId txidHash
    Nothing -> Prelude.error "Could not derserialize txid"

-- Minting script that checks the minting value, validty interval and
-- required signers in the ScriptContext is equivalent to what's in the
-- redeemer.

{-# INLINABLE mkPolicy #-}
mkPolicy :: MyCustomRedeemer -> Plutus.ScriptContext -> Bool
mkPolicy (MyCustomRedeemer _ _ minted txValidRange _fee _ _ signatories) scriptContext =
  -- Minted value is equivalent
  minted P.== Plutus.txInfoMint txInfo P.&&
  -- Validity range is equivalent
  Plutus.txInfoValidRange txInfo P.== txValidRange P.&&
  -- Required signers are equivalent
  AMap.member singleSignatory scriptContextSignatoriesMap
 where
   scriptContextSignatoriesMap :: AMap.Map Plutus.PubKeyHash Integer
   scriptContextSignatoriesMap = AMap.fromList P.$ P.zip (Plutus.txInfoSignatories txInfo) [1]

   singleSignatory :: Plutus.PubKeyHash
   singleSignatory = P.head signatories

   txInfo :: Plutus.TxInfo
   txInfo = Plutus.scriptContextTxInfo scriptContext

policy :: Scripts.MintingPolicy
policy = Plutus.mkMintingPolicyScript
           $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapMintingPolicy mkPolicy

plutusMintingScript :: Plutus.Script
plutusMintingScript =
  Plutus.unMintingPolicyScript policy

mintingValidator :: Plutus.Validator
mintingValidator =
  Plutus.Validator $ Plutus.unMintingPolicyScript policy

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise mintingValidator

customApiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
customApiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
