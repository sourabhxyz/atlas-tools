module AtlasTools.Send (
  sendAllTo,
) where

import Control.Exception (Exception)
import Control.Monad (when)
import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.TxBuilder
import GeniusYield.Types

data SendAllLessThan2Ada = SendAllLessThan2Ada
  deriving stock (Show)
  deriving anyclass (Exception, IsGYApiError)

-- | Send all funds from one address to another address except 2 ADA, provided total funds are greater than or equal to 2 ADA and raises exception otherwise.
sendAllTo :: (GYTxQueryMonad m) => GYAddress -> Maybe GYTxOutRef -> GYAddress -> m (GYTxSkeleton v)
sendAllTo fromAddr mreserveColl toAddr = do
  logMsg mempty GYInfo $ "Sending all funds from " <> show fromAddr <> " with optional collateral being " <> show mreserveColl <> " to " <> show toAddr
  ownUtxos <- utxosAtAddress fromAddr Nothing
  logMsg mempty GYInfo $ "Own UTXOs: " <> show ownUtxos
  let totalValue = foldMapUTxOs (\u -> if Just (utxoRef u) == mreserveColl then mempty else utxoValue u) ownUtxos
      adaBuffer = valueFromLovelace 2_000_000
  logMsg mempty GYInfo $ "Total value: " <> show totalValue
  when (adaBuffer `valueGreater` totalValue) $ throwAppError SendAllLessThan2Ada
  pure $ mustHaveOutput $ mkGYTxOutNoDatum toAddr (totalValue <> valueNegate adaBuffer)