module AtlasTools.Utils where
  

import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId), coreConfigIO, withCfgProviders)
import GeniusYield.TxBuilder (runGYTxBuilderMonadIO, runGYTxQueryMonadIO, GYTxBuilderMonadIO)
import GeniusYield.TxBuilder.Class
import GeniusYield.Types

runGYTxMonadNode :: GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO (GYTxSkeleton v) -> IO GYTxBody
runGYTxMonadNode nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBody