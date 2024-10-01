{-# OPTIONS_GHC -Wno-orphans #-}
module AtlasTools.Orphans where


import GeniusYield.TxBuilder (GYTxBuilderMonadIO, GYTxQueryMonadIO)
import           GeniusYield.TxBuilder.IO.Unsafe                    (unsafeIOToQueryMonad, unsafeIOToTxBuilderMonad)
import Control.Monad.IO.Class (MonadIO (..))


instance MonadIO GYTxQueryMonadIO where
    liftIO = unsafeIOToQueryMonad

instance MonadIO GYTxBuilderMonadIO where
    liftIO = unsafeIOToTxBuilderMonad