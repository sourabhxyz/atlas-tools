{-# LANGUAGE DuplicateRecordFields #-}

module AtlasTools.Options (
  Command (..),
  SendAllToCfg (..),
  PosixTimeToSlotCfg (..),
  parseCommand,
) where

import Data.Time.Clock.POSIX (POSIXTime)
import GeniusYield.Types
import Options.Applicative
import Text.Read (readMaybe)

data Command = SendAllTo !SendAllToCfg | PosixTimeToSlot !PosixTimeToSlotCfg

-- >>> read "1719314270198" :: POSIXTime

data SendAllToCfg = SendAllToCfg
  { fromAddr :: !GYAddressBech32
  , toAddr :: !GYAddressBech32
  , fromAddrPaymentKeyPath :: !FilePath
  , atlasConfig :: !FilePath
  , mwriteTx :: !(Maybe FilePath)
  , dryRun :: !Bool
  , mcollateralToReserve :: !(Maybe GYTxOutRef)
  }
  deriving stock (Show)

parseSendAllToCfg :: Parser SendAllToCfg
parseSendAllToCfg =
  SendAllToCfg
    <$> strOption (metavar "FROM_ADDR" <> help "Address to send all funds from" <> short 'f' <> long "from-addr")
    <*> strOption (metavar "TO_ADDR" <> help "Address to send all funds to" <> short 't' <> long "to-addr")
    <*> strOption (metavar "FROM_ADDR_PAYMENT_KEY_PATH" <> help "Payment signing key filepath for FROM_ADDR" <> short 'k' <> long "from-addr-payment-key-path")
    <*> strOption (metavar "ATLAS_CONFIG" <> help "Atlas config filepath" <> short 'c' <> long "atlas-config")
    <*> optional (strOption (metavar "WRITE_TX" <> help "Write transaction to file" <> short 'w' <> long "write-tx"))
    <*> switch (long "dry-run" <> help "Dry run mode (will not submit the built transaction)" <> short 'd')
    <*> optional (strOption (metavar "OUTPUT_TO_RESERVE" <> help "UTxO to reserve" <> short 'r' <> long "output-to-reserve"))

data PosixTimeToSlotCfg = PosixTimeToSlotCfg
  { posixTime :: !POSIXTime
  , atlasConfig :: !FilePath
  }
  deriving stock (Show)

parsePosixTime :: String -> Maybe POSIXTime
parsePosixTime s = do
  timestampMillis <- readMaybe s :: Maybe Integer
  return (fromInteger timestampMillis / 1000)

parsePosixTimeToSlotCfg :: Parser PosixTimeToSlotCfg
parsePosixTimeToSlotCfg =
  PosixTimeToSlotCfg
    <$> option (maybeReader parsePosixTime) (metavar "POSIX_TIME" <> help "POSIX time to convert to slot number" <> short 'p' <> long "posix-time")
    <*> strOption (metavar "ATLAS_CONFIG" <> help "Atlas config filepath" <> short 'c' <> long "atlas-config")

parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ command "send-all-to" $ info (SendAllTo <$> parseSendAllToCfg <**> helper) $ progDesc "Send all funds from one address to another address, provided total funds are greater than or equal to 2 ADA and raises exception otherwise."
      , command "posix-time-to-slot" $ info (PosixTimeToSlot <$> parsePosixTimeToSlotCfg <**> helper) $ progDesc "Convert POSIX time to slot number."
      ]