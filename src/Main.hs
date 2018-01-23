{-# language StrictData #-}
{-# language TypeApplications #-}

module Main where

import Servant
import Control.Monad (when)
import Network.Haskoin.Script (ScriptOutput(..), scriptAddr)
import Network.Haskoin.Internals (switchToTestnet3)
import Options.Applicative
import Data.Semigroup ((<>))
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import Signer.Server
import Signer.Config

data CmdOptions = CmdOptions {
  port       :: Int,
  testnet    :: Bool,
  configFile :: FilePath
}

main :: IO ()
main = do

  options <- execParser cmdOptionsParser

  when (testnet options)
    switchToTestnet3

  cfg <- getConfig (configFile options)

  checkSecurity (testnet options) (configFile options)

  let scrAddr = scriptAddr $ redeemScript cfg

  run (port options) $ logStdoutDev $ serve (Proxy @API)
    $ server scrAddr (PayScriptHash scrAddr) cfg

cmdOptionsParser :: ParserInfo CmdOptions
cmdOptionsParser =
  info
    (CmdOptions
      <$> option auto
        ( long "port"
      <> short 'p'
      <> metavar "PORT#"
      <> help "Port number to listen on"
      <> showDefault
      <> value 8080
        )
      <*> switch
        ( long "testnet"
      <> short 't'
      <> help "Testnet mode"
        )
      <*> strOption
        ( long "config"
      <> short 'c'
      <> metavar "FILE"
      <> help "Config file location"
      <> showDefault
      <> value "./multisigner.yaml"
        ) <**> helper)
    fullDesc
