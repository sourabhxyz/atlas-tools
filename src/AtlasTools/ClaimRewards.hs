module AtlasTools.ClaimRewards (
  claimRewards,
) where

import Control.Exception (Exception)
import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.TxBuilder
import GeniusYield.Types

data ClaimRewardsStakeInfoNotFound = ClaimRewardsStakeInfoNotFound
  deriving stock (Show)
  deriving anyclass (Exception, IsGYApiError)

-- | Claim rewards from a stake address.
claimRewards ::
  (GYTxQueryMonad m) =>
  -- | Address whose UTxOs would be used to build withdrawal transaction.
  GYAddress ->
  -- | Optional collateral to reserve.
  Maybe GYTxOutRef ->
  -- | Stake address to claim rewards from.
  GYStakeAddress ->
  -- | Address to send claimed rewards to.
  GYAddress ->
  m (GYTxSkeleton v)
claimRewards fromAddr mreserveColl stakeAddr toAddr = do
  logMsg mempty GYInfo $ "Address to assist tx building: " <> show fromAddr <> ", with optional collateral being: " <> show mreserveColl <> " and stake address to claim rewards from being: " <> show stakeAddr <> ". Claimed rewards would be sent to: " <> show toAddr
  msai <- stakeAddressInfo stakeAddr
  logMsg mempty GYInfo $ "Stake address information: " <> show msai
  case msai of
    Nothing -> throwAppError ClaimRewardsStakeInfoNotFound
    Just GYStakeAddressInfo{..} ->
      pure $ mustHaveWithdrawal (GYTxWdrl stakeAddr gyStakeAddressInfoAvailableRewards GYTxWdrlWitnessKey) <> mustHaveOutput (mkGYTxOutNoDatum toAddr (valueFromLovelace $ fromIntegral gyStakeAddressInfoAvailableRewards))