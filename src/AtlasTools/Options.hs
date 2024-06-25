module AtlasTools.Options (
  Command (..),
  SendAllToCfg (..),
  parseCommand,
) where

import GeniusYield.Types
import Options.Applicative

data Command = SendAllTo !SendAllToCfg

data SendAllToCfg = SendAllToCfg
  { fromAddr :: !GYAddressBech32
  , toAddr :: !GYAddressBech32
  , fromAddrPaymentKeyPath :: !FilePath
  , atlasConfig :: !FilePath
  , mwriteTx :: !(Maybe FilePath)
  , dryRun :: !Bool
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

parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ command "send-all-to" $ info (SendAllTo <$> parseSendAllToCfg <**> helper) $ progDesc "Send all funds from one address to another address, provided total funds are greater than or equal to 2 ADA and raises exception otherwise."
      ]