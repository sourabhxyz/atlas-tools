module AtlasTools.Run (
  run,
) where

import AtlasTools.Options (Command (..), PosixTimeToSlotCfg (..), SendAllToCfg (..), parseCommand)
import AtlasTools.Send
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromJust, isJust)
import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId), coreConfigIO, withCfgProviders)
import GeniusYield.TxBuilder (runGYTxQueryMonadIO)
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
import Options.Applicative
import AtlasTools.Utils (runGYTxMonadNode)
import AtlasTools.Orphans ()

run :: IO ()
run = runCommand =<< execParser opts
 where
  opts =
    info
      (parseCommand <**> helper)
      ( fullDesc
          <> progDesc "Simple CLI utilities making use of Atlas under the hood"
          <> header "Atlas Tools"
      )

runCommand :: Command -> IO ()
runCommand (SendAllTo cfg@SendAllToCfg{..}) = do
  coreCfg <- coreConfigIO atlasConfig
  let nid = cfgNetworkId coreCfg
  pkey <- readPaymentSigningKey fromAddrPaymentKeyPath
  withCfgProviders coreCfg "AtlasTools" $ \provider -> do
    gyLogInfo provider mempty $ "Parsed config: " <> show cfg <> "\n"
    let fromAddr' = addressFromBech32 fromAddr
        toAddr' = addressFromBech32 toAddr
    txBody <- runGYTxMonadNode nid provider [fromAddr'] toAddr' (mcollateralToReserve >>= \r -> Just (r, False)) $ sendAllTo fromAddr' mcollateralToReserve toAddr'
    runGYTxQueryMonadIO nid provider $ do
      logMsg mempty GYInfo $ "Generated transaction body: " <> show txBody
      let tx = signGYTxBody txBody [pkey]
      liftIO $ when (isJust mwriteTx) $ writeTx (fromJust mwriteTx) tx
      unless dryRun $ do
        txId <- liftIO $ gySubmitTx provider tx
        logMsg mempty GYInfo $ "Submitted transaction with ID: " <> show txId
runCommand (PosixTimeToSlot cfg@PosixTimeToSlotCfg{..}) = do
  coreCfg <- coreConfigIO atlasConfig
  let nid = cfgNetworkId coreCfg
  withCfgProviders coreCfg "AtlasTools" $ \provider -> do
    gyLogInfo provider mempty $ "Parsed config: " <> show cfg <> "\n"
    let gytime = timeFromPOSIX posixTime
    ms <- runGYTxQueryMonadIO nid provider $ enclosingSlotFromTime gytime
    gyLogInfo provider mempty $ "Slot number: " <> show ms