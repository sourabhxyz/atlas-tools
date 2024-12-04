module AtlasTools.Utils where

import GeniusYield.TxBuilder (GYTxBuilderMonadIO, runGYTxBuilderMonadIO)
import GeniusYield.TxBuilder.Class
import GeniusYield.Types

runGYTxMonadNode :: GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO (GYTxSkeleton v) -> IO GYTxBody
runGYTxMonadNode nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBody